package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.{ByteArrayInputStream, File, ObjectInputStream, ObjectOutputStream}

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.{NoInfoEdgeLabel, Solve, Tactic}
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{Unknown, VerifierStatus}
import jetbrains.exodus.bindings.ComparableBinding
import jetbrains.exodus.entitystore._
import jetbrains.exodus.util.LightOutputStream

import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import scala.language.implicitConversions

/**
  * Class for making any type ordered, if desired (used by ProofGraphXodus implementation for writing objects into
  * the database)
 */
class OrderedWrapper[T](val obj: T) extends Ordered[T] {

  def compare(that: T): Int = {
    val hcompare = this.hashCode compare that.hashCode
    if (hcompare != 0)
      return hcompare
    if (this equals that)
      return 0
    throw new RuntimeException(s"Failed to compare $this and $that using hash codes (happens if objects are different but still return the same hash codes!)")
  }

  override def equals(cobj: scala.Any): Boolean = cobj match {
    case ord: OrderedWrapper[T] => obj equals ord.obj
    case _ => super.equals(cobj)
  }

  override def hashCode(): Int = obj.hashCode()
}

class ProofGraphXodus[Spec, Goal](dbDir: File) extends ProofGraph[Spec, Goal] {

  import ProofGraphXodus._

  //make a type Ordered if necessary - should be called whenever a type that does not implement Comparable/Ordered
  //shall be written into the database
  //if makeOrdered shall really be implicit, make sure that types that are already Ordered are not wrapped again
  //currently, makeOrdered should only ever be called for stuff that is not Ordered
  def makeOrdered[T](t: T): OrderedWrapper[T] = new OrderedWrapper[T](t)

  //read an ordered wrapper with the given type T inside out of the database
  //has to be called when reading an object out of the database that was wrapped in OrderedWrapper when written into the database
  def readOrdered[T](to: Comparable[_]): T = to.asInstanceOf[OrderedWrapper[T]].obj


  val store: PersistentEntityStore = PersistentEntityStores.newInstance(dbDir)
  //PropertyTypes.registerAll(store)

  private val shutdown = new Thread(new Runnable {
    override def run() = store.close()
  })
  Runtime.getRuntime.addShutdownHook(shutdown)

  def close(): Unit = {
    Runtime.getRuntime.removeShutdownHook(shutdown)
    store.close()
  }

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


  protected trait EntityObj {
    def id: EntityId

    def entity(txn: StoreTransaction): Entity = txn.getEntity(id)
  }

  class ProofStep(val id: EntityId, val tactic: Tactic[Spec, Goal]) extends GenProofStep[Spec, Goal] with EntityObj {
    def this(id: EntityId, entity: Entity) =
      this(id, readOrdered[Tactic[Spec, Goal]](entity.getProperty(pStepTactic)))

    def this(id: EntityId, txn: StoreTransaction) = this(id, txn.getEntity(id))

    def canEqual(other: Any): Boolean = other.isInstanceOf[ProofStep]

    override def equals(other: Any): Boolean = other match {
      case that: ProofStep =>
        (that canEqual this) &&
          id == that.id &&
          tactic == that.tactic
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(id, tactic)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }


  class Obligation(val id: EntityId, val spec: Spec, val goal: Goal) extends GenObligation[Spec, Goal] with EntityObj {
    def this(id: EntityId, entity: Entity) =
      this(id, readOrdered[Spec](entity.getLink(lOblSpec).getProperty(pSpecContent)),
            readOrdered[Goal](entity.getProperty(pOblGoal)))


    def this(id: EntityId, txn: StoreTransaction) = this(id, txn.getEntity(id))
  }

  object obligationProducer extends ObligationProducer[Spec, Goal, Obligation] {
    override def newObligation(spec: Spec, goal: Goal): Obligation =
      transaction[Obligation](txn => newObligationST(txn, spec, goal))

    def newObligationST(txn: StoreTransaction, specObj: Spec, goalObj: Goal): Obligation = {
      // TODO maybe improve performance through index Spec->Entity, but maybe Xodus does that already or this is not performance-critical anyways
      val ordered_specObj = makeOrdered(specObj)
      val ordered_goalObj = makeOrdered(goalObj)

      val existing = txn.find(TSpec, pSpecContent, ordered_specObj).asScala
      val spec = existing.headOption.getOrElse {
        val spec = txn.newEntity(TSpec)
        spec.setProperty(pSpecContent, ordered_specObj)
        spec
      }

      // find exisiting obl or create a new one
      val existingObls = txn.find(TObligation, pOblGoal, ordered_goalObj).asScala
      val withSpecLinked = existingObls.find { entity =>
        val spec = readOrdered[Spec](entity.getLink(lOblSpec).getProperty(pSpecContent))
        spec == specObj
      }
      val obl = withSpecLinked.headOption.getOrElse {
        val obl = txn.newEntity(TObligation)
        obl.setProperty(pOblGoal, ordered_goalObj)
        obl.setLink(lOblSpec, spec)
        obl
      }
      new Obligation(obl.getId, specObj, goalObj)
    }
  }


  class StepResult(val id: EntityId, val status: VerifierStatus[Spec, Goal], val evidence: Option[Evidence], val errorMsg: Option[String]) extends GenStepResult[Spec, Goal] with EntityObj {
    def this(id: EntityId, entity: Entity) =
      this(
        id,
        readOrdered[VerifierStatus[Spec, Goal]](entity.getProperty(pResultStatus)),
        fromNullable(readOrdered[Evidence](entity.getProperty(pResultEvidence))),
        fromNullable(entity.getProperty(pResultErrorMsg).asInstanceOf[String])
      )

    def this(id: EntityId, txn: StoreTransaction) = this(id, txn.getEntity(id))
  }

  object stepResultProducer extends StepResultProducer[Spec, Goal, StepResult] {
    override def newStepResult(status: VerifierStatus[Spec, Goal], evidence: Option[Evidence], errorMsg: Option[String]): StepResult =
      transaction(txn => newStepResult(txn, status, evidence, errorMsg))

    def newStepResult(txn: StoreTransaction, status: VerifierStatus[Spec, Goal], evidence: Option[Evidence], errorMsg: Option[String]): StepResult = {
      val result = txn.newEntity(TStepResult)
      result.setProperty(pResultStatus, makeOrdered(status))
      if (errorMsg.isDefined)
        result.setProperty(pResultErrorMsg, errorMsg.get)
      if (evidence.isDefined)
        result.setProperty(pResultEvidence, makeOrdered(evidence.get))
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
    val edgeLabel = this.requiringSteps(targetObj) map (_._2)

    val requiredObjs = transaction { txn =>
      // execute tactic within transaction so that a tactic failure unrolls any changes made so far
      tactic(targetObj, edgeLabel, obligationProducer)
    }

    //TODO: code below throws NullPointerExceptions when one tries to actually overwrite an already existing proof step!
    //currently, don't try overwriting proof steps
    val newOrRetainedEdges = transaction { txn =>
      val obl = targetObj.entity(txn)
      val step = obl.getLink(lOblAppliedStep)

      val oldRequiredObjs = if (step != null) {
        obl.deleteLink(lOblAppliedStep, step)
        step.deleteLink(lStepTargetedObl, obl)

        if (step != null) {
          val result = step.getLink(lStepVerifiedBy)
          // delete result if exists
          if (result != null)
            result.delete()
          // delete all required edges and gc all required obligations
          val edges = step.getLinks(lStepRequiredEdges).asScala.toSeq
          step.delete() //????? should this be deleted? sometimes throws strange NullPointerExceptions
          edges.map { edge =>
            val obl = edge.getLink(lEdgeRequiredObl)
            edge.delete() //???? should this be deleted? sometimes throws strange NullPointerExceptions
            (new Obligation(obl.getId(), obl), readOrdered[EdgeLabel](edge.getProperty(pEdgeLabel)))
          }
        }
        else
          Seq()
      }
      else
        Seq()

      var retainedEdges = Set[(Obligation, EdgeLabel)]()
      val newOrRetainedEdges = requiredObjs map { case (obl, label) =>
        val found = oldRequiredObjs.find(old => old._2 == label && old._1.goal == obl.goal && old._1.spec == obl.spec)
        found match {
          case None => (obl, label)
          case Some(old) =>
            gcObl(obl.entity(txn))
            retainedEdges += old
            old
        }
      }

      oldRequiredObjs foreach { old =>
        if (!retainedEdges.contains(old))
          gcObl(old._1.entity(txn))
      }
      newOrRetainedEdges
    }

    transaction { txn =>
      val target = targetObj.entity(txn)
      val step = txn.newEntity(TProofStep)
      step.setProperty(pStepTactic, makeOrdered(tactic))
      step.setLink(lStepTargetedObl, target)
      target.setLink(lOblAppliedStep, step)

      for ((requiredObj, label) <- newOrRetainedEdges) {
        val required = requiredObj.entity(txn)
        val edge = txn.newEntity(TEdge)
        edge.setProperty(pEdgeLabel, makeOrdered(label))

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
      val label = readOrdered[EdgeLabel](edge.getProperty(pEdgeLabel))
      requiredObl -> label
    }
  }

  /** Yields proof steps that require the given obligation */
  def requiringSteps(obl: Obligation): Iterable[(ProofStep, EdgeLabel)] = readOnlyTransaction { txn =>
    // copy all edges to prevent capture of transaction within result
    val edges = Seq() ++ obl.entity(txn).getLinks(lOblRequiringEdges).asScala
    edges.map { edge =>
      val requiringStep = new ProofStep(edge.getLink(lEdgeRequiringStep).getId, txn)
      val label = readOrdered[EdgeLabel](edge.getProperty(pEdgeLabel))
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
    Option(v)


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

  /**
    * This function has to be statically called once for every ProofGraphXodus instance and every type
    * T <: Comparable[_] which shall be written in the database
    * in the current setup, it should not be necessary anymore to call this function for any type, since all
    * types will be wrapped in OrderedWrapper automatically
    * @param store
    * @param tag
    * @tparam T
    */
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

  /**
    * this function has to be called once for every ProofGraphXodus instance
    * it registers the OrderedWrapper as property for the database, which wraps all the types of objects that
    * shall be written into the database
    *
    * @param store
    */
  def registerWrapperType(store: PersistentEntityStore) =
    store.executeInTransaction(new StoreTransactionalExecutable() {
      override def execute(txn: StoreTransaction): Unit = {
        val binding = new ComparableBinding {
          override def writeObject(output: LightOutputStream, obj: Comparable[_]) = {
            val cobj = obj.asInstanceOf[OrderedWrapper[_]]
            val oos = new ObjectOutputStream(output)
            oos.writeObject(cobj.obj)
          }

          override def readObject(stream: ByteArrayInputStream) = {
            val ois = new ObjectInputStream(stream)
            new OrderedWrapper(ois.readObject())
          }
        }

        val tag = implicitly [ClassTag[OrderedWrapper[_]]]
        val clazz = tag.runtimeClass.asInstanceOf[Class[_ <: Comparable[_]]]
        store.registerCustomPropertyType(txn, clazz, binding)
      }
    })
}
