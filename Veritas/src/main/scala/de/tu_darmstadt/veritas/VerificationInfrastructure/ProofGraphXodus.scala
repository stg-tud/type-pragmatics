package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.{ByteArrayInputStream, File, ObjectInputStream, ObjectOutputStream}

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.{NoInfoEdgeLabel, Solve, Tactic}
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{Unknown, VerifierStatus}
import jetbrains.exodus.bindings.ComparableBinding
import jetbrains.exodus.entitystore._
import jetbrains.exodus.util.LightOutputStream

import scala.collection.JavaConverters._
import scala.reflect.ClassTag


class ProofGraphXodus[Spec <: Comparable[Spec], Goal <: Comparable[Goal]](dbDir: File) extends ProofGraph[Spec, Goal] {

  import ProofGraphXodus._

  val store: PersistentEntityStore = PersistentEntityStores.newInstance(dbDir)
  PropertyTypes.registerAll(store)

  // TODO add an index Step->EntityId for faster lookups

  def transaction[T](f: StoreTransaction => T): T = {
    var result: Option[T] = None
    store.executeInTransaction(new StoreTransactionalExecutable {
      override def execute(txn: StoreTransaction) = {
        result = Some(f(txn))
      }
    })
    result.get
  }

  def readOnlyTransaction[T](f: StoreTransaction => T): T = {
    var result: Option[T] = None
    store.executeInReadonlyTransaction(new StoreTransactionalExecutable {
      override def execute(txn: StoreTransaction) = {
        result = Some(f(txn))
      }
    })
    result.get
  }


  trait EntityObj {
    def id: EntityId

    def entity(txn: StoreTransaction): Entity = txn.getEntity(id)
  }

  class ProofStep(val id: EntityId, val tactic: Tactic[Spec, Goal]) extends GenProofStep[Spec, Goal] with EntityObj {
    def this(id: EntityId, entity: Entity) =
      this(id, entity.getProperty(pStepTactic).asInstanceOf[Tactic[Spec, Goal]])

    def this(id: EntityId, txn: StoreTransaction) = this(id, txn.getEntity(id))
  }


  class Obligation(val id: EntityId, val spec: Spec, val goal: Goal) extends GenObligation[Spec, Goal] with EntityObj {
    def this(id: EntityId, entity: Entity) =
      this(id, entity.getLink(lOblSpec).getProperty(pSpecContent).asInstanceOf[Spec],
            entity.getProperty(pOblGoal).asInstanceOf[Goal])


    def this(id: EntityId, txn: StoreTransaction) = this(id, txn.getEntity(id))
  }

  object obligationProducer extends ObligationProducer[Spec, Goal, Obligation] {
    override def newObligation(spec: Spec, goal: Goal): Obligation =
      transaction[Obligation](txn => newObligationST(txn, spec, goal))

    //TODO: Why does producing a new obligation already insert the obligation into the data base, even if we did not yet decide to store it?!

    def newObligationST(txn: StoreTransaction, specObj: Spec, goalObj: Goal): Obligation = {
      //TODO ensure that we don't store the same specification multiple times?
      val spec = txn.newEntity(TSpec)
      spec.setProperty(pSpecContent, specObj)

      val obl = txn.newEntity(TObligation)
      obl.setProperty(pOblGoal, goalObj)
      obl.setLink(lOblSpec, spec)
      new Obligation(obl.getId, specObj, goalObj)
    }
  }


  class StepResult(val id: EntityId, val status: VerifierStatus[Spec, Goal], val evidence: Option[Evidence], val errorMsg: Option[String]) extends GenStepResult[Spec, Goal] with EntityObj {
    def this(id: EntityId, entity: Entity) =
      this(
        id,
        entity.getProperty(pResultStatus).asInstanceOf[VerifierStatus[Spec, Goal]],
        fromNullable(entity.getProperty(pResultEvidence).asInstanceOf[Evidence]),
        fromNullable(entity.getProperty(pResultErrorMsg).asInstanceOf[String])
      )

    def this(id: EntityId, txn: StoreTransaction) = this(id, txn.getEntity(id))
  }

  object stepResultProducer extends StepResultProducer[Spec, Goal, StepResult] {
    override def newStepResult(status: VerifierStatus[Spec, Goal], evidence: Option[Evidence], errorMsg: Option[String]): StepResult =
      transaction(txn => newStepResult(txn, status, evidence, errorMsg))

    def newStepResult(txn: StoreTransaction, status: VerifierStatus[Spec, Goal], evidence: Option[Evidence], errorMsg: Option[String]): StepResult = {
      val result = txn.newEntity(TStepResult)
      result.setProperty(pResultStatus, status)
      if (errorMsg.isDefined)
        result.setProperty(pResultErrorMsg, errorMsg.get)
      if (evidence.isDefined)
        result.setProperty(pResultEvidence, evidence.get)
      new StepResult(result.getId, status, evidence, errorMsg)
    }
  }


  /* operations for modifying proof graphs:
   * - add or remove root obligations
   * - apply or unapply a tactic to an obligation, yielding a proof step and subobligations
   * - setting or unsetting the result of validating a proof step
   */


  def storeObligation(name: String, oblObj: Obligation) = transaction { txn =>
    val old = txn.find(TStored, pStoredName, name)
    if (old.isEmpty) {
      val stored = txn.newEntity(TStored)
      val obl = oblObj.entity(txn)
      stored.setProperty(pStoredName, name)
      stored.setLink(lStoredObl, obl)
      obl.setLink(lOblRoot, stored)
      None
    }
    else {
      val stored = old.getFirst
      val oldObl = stored.getLink(lStoredObl)
      oldObl.deleteLink(lOblRoot, stored)
      val obl = oblObj.entity(txn)
      stored.setLink(lStoredObl, obl)
      obl.setLink(lOblRoot, stored)
      Some(new Obligation(oldObl.getId, txn))
    }
  }

  def unstoreObligation(oblObj: Obligation) = transaction { txn =>
    val obl = oblObj.entity(txn)
    val stored = obl.getLink(lOblRoot)
    if (stored != null)
      stored.delete()
    gcObl(obl)
  }

  def applyTactic(targetObj: Obligation, tactic: Tactic[Spec, Goal]): ProofStep = {
    val requiredObjs = tactic(targetObj, obligationProducer)

    transaction { txn =>
      val target = targetObj.entity(txn)
      val step = txn.newEntity(TProofStep)
      step.setProperty(pStepTactic, tactic)
      step.setLink(lStepTargetedObl, target)
      target.setLink(lOblAppliedStep, step)

      for ((requiredObj, label) <- requiredObjs) {
        val required = requiredObj.entity(txn)
        val edge = txn.newEntity(TEdge)
        edge.setProperty(pEdgeLabel, label)

        edge.setLink(lEdgeRequiredObl, required)
        required.addLink(lOblRequiringEdges, edge)

        edge.setLink(lEdgeRequiringStep, step)
        step.addLink(lStepRequiredEdges, edge)
      }
      new ProofStep(step.getId, tactic)
    }
  }

  def unapplyTactic(oblObj: Obligation) = transaction { txn =>
    val obl = oblObj.entity(txn)
    val step = obl.getLink(lOblAppliedStep)
    if (step != null) {
      obl.deleteLink(lOblAppliedStep, step)
      step.deleteLink(lStepTargetedObl, obl)
      gcStep(step)
    }
  }

  def setVerifiedBy(step: ProofStep, resultObj: StepResult) = transaction { txn =>
    step.entity(txn).setLink(lStepVerifiedBy, resultObj.entity(txn))
  }

  def unsetVerifiedBy(stepObj: ProofStep) = transaction { txn =>
    val step = stepObj.entity(txn)
    val result = step.getLink(lStepVerifiedBy)
    if (result != null) {
      step.deleteLink(lStepVerifiedBy, result)
      result.delete()
    }
  }


  /** operations for querying proof graphs:
    * - sequence of root proof obligations
    * - navigating from obligations to used proof step to required subobligations
    * - navigating from subobligations to requiring proof steps to targeted obligation
    * - retrieving step result if any and checking whether a step was successfully verified
    * - checking whether an obligation was successfully verified
    */

  def storedObligations: Map[String, Obligation] = readOnlyTransaction { txn =>
    Map() ++ txn.getAll(TStored).asScala.map(stored =>
      stored.getProperty(pStoredName).asInstanceOf[String] -> new Obligation(stored.getLink(lStoredObl).getId, txn)
    )
  }

  override def findObligation(name: String): Option[Obligation] = readOnlyTransaction { txn =>
    val stored = txn.find(TStored, pStoredName, name)
    if (stored.isEmpty)
      None
    else
      Some(new Obligation(stored.getFirst.getLink(lStoredObl).getId, txn))
  }

  /** Yields proof step if any */
  def appliedStep(obl: Obligation): Option[ProofStep] = readOnlyTransaction { txn =>
    val step = obl.entity(txn).getLink(lOblAppliedStep)
    if (step == null)
      None
    else
      Some(new ProofStep(step.getId, txn))
  }

  /** Yields required subobligations */
  def requiredObls(step: ProofStep): Iterable[(Obligation, EdgeLabel)] = readOnlyTransaction { txn =>
    // copy all edges to prevent capture of transaction within result
    val edges = Seq() ++ step.entity(txn).getLinks(lStepRequiredEdges).asScala
    edges.map { edge =>
      val requiredObl = new Obligation(edge.getLink(lEdgeRequiredObl).getId, txn)
      val label = edge.getProperty(pEdgeLabel).asInstanceOf[EdgeLabel]
      requiredObl -> label
    }
  }

  /** Yields proof steps that require the given obligation */
  def requiringSteps(obl: Obligation): Iterable[(ProofStep, EdgeLabel)] = readOnlyTransaction { txn =>
    // copy all edges to prevent capture of transaction within result
    val edges = Seq() ++ obl.entity(txn).getLinks(lOblRequiringEdges).asScala
    edges.map { edge =>
      val requiringStep = new ProofStep(edge.getLink(lEdgeRequiringStep).getId, txn)
      val label = edge.getProperty(pEdgeLabel).asInstanceOf[EdgeLabel]
      requiringStep -> label
    }
  }

  /** Yields the obligation the proof step was applied to */
  def targetedObl(step: ProofStep): Obligation = readOnlyTransaction { txn =>
    new Obligation(step.entity(txn).getLink(lStepTargetedObl).getId, txn)
  }

  def verifiedBy(step: ProofStep): Option[StepResult] = readOnlyTransaction { txn =>
    val result = step.entity(txn).getLink(lStepVerifiedBy)
    if (result == null)
      None
    else {
      Some(new StepResult(result.getId, txn))
    }
  }

  def fromNullable[T <: AnyRef](v: T): Option[T] =
    if (v == null)
      None
    else
      Some(v)


  /* GC helpers */

  private def gcObl(obl: Entity): Unit = {
    if (obl != null && obl.getLink(lOblRoot) == null && obl.getLinks(lOblRequiringEdges).isEmpty) {
      // no root ref and no requiring edges => can delete
      obl.getLink(lOblSpec).delete()
      val step = obl.getLink(lOblAppliedStep)
      // delete step if exists
      if (step != null)
        step.deleteLink(lStepTargetedObl, obl)
      gcStep(step)
      obl.delete()
    }
  }

  private def gcStep(step: Entity): Unit = {
    if (step != null && step.getLink(lStepTargetedObl) == null) {
      val result = step.getLink(lStepVerifiedBy)
      // delete result if exists
      if (result != null)
        result.delete()
      // delete all required edges and gc all required obligations
      for (edge <- step.getLinks(lStepRequiredEdges).asScala) {
        val requiredObl = edge.getLink(lEdgeRequiredObl)
        requiredObl.deleteLink(lOblRequiringEdges, edge)
        gcObl(requiredObl)
        edge.delete()
      }
      step.delete()
    }
  }

}

object ProofGraphXodus {
  val TStored = "ROOT"
  val pStoredName = "name"
  val lStoredObl = "obl"

  val TObligation = "OBLIGATION"
  val pOblGoal = "goal"
  val lOblSpec = "spec"
  val lOblAppliedStep = "appliedStep"
  val lOblRequiringEdges = "requiringEdges"
  val lOblRoot = "root"

  val TProofStep = "PROOFSTEP"
  val pStepTactic = "tactic"
  val lStepRequiredEdges = "requiredEdges"
  val lStepTargetedObl = "targetedObl"
  val lStepVerifiedBy = "verifiedBy"

  val TEdge = "EDGE"
  val pEdgeLabel = "label"
  val lEdgeRequiredObl = "requiredObl"
  val lEdgeRequiringStep = "requiringStep"

  val TSpec = "SPEC"
  val pSpecContent = "content"

  val TStepResult = "STEPRESULT"
  val pResultStatus = "status"
  val pResultEvidence = "evidence"
  val pResultErrorMsg = "errorMsg"
}

object PropertyTypes {
  def registerAll(store: PersistentEntityStore) {
    registerPropertyType[Solve[_, _]](store)
    registerPropertyType[NoInfoEdgeLabel.type](store)
    registerPropertyType[VerifierStatus[_, _]](store)
    registerPropertyType[Unknown[_, _]](store)
  }


  def registerPropertyType[T <: Comparable[_] with Serializable](store: PersistentEntityStore)
                                              (implicit tag: ClassTag[T]) =
    store.executeInTransaction(new StoreTransactionalExecutable() {
      override def execute(txn: StoreTransaction): Unit = {
        val binding = new ComparableBinding {
          override def writeObject(output: LightOutputStream, obj: Comparable[_]) = {
            val oos = new ObjectOutputStream(output)
            oos.writeObject(obj)
          }

          override def readObject(stream: ByteArrayInputStream) = {
            val ois = new ObjectInputStream(stream)
            ois.readObject().asInstanceOf[T]
          }
        }

        val clazz = tag.runtimeClass.asInstanceOf[Class[_ <: Comparable[_]]]
        store.registerCustomPropertyType(txn, clazz, binding)
      }
    })
}
