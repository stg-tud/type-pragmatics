package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.{ByteArrayInputStream, File}

import de.tu_darmstadt.veritas.VerificationInfrastructure
import de.tu_darmstadt.veritas.VerificationInfrastructure.Evidence.AnyEvidenceChecker
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.{NoInfoEdgeLabel, Solve, Tactic}
import jetbrains.exodus.bindings.ComparableBinding
import jetbrains.exodus.entitystore._
import jetbrains.exodus.util.LightOutputStream

import scala.collection.JavaConverters._
import scala.pickling.binary.BinaryPickleStream
import scala.pickling.{FastTypeTag, OutputStreamOutput, SPickler, Unpickler}
import scala.reflect.ClassTag


class ProofGraphXodus[Spec <: Comparable[Spec], Goal <: Comparable[Goal]](dbDir: File) extends ProofGraph[Spec, Goal] {
  import ProofGraphXodus._

  val store: PersistentEntityStore = PersistentEntityStores.newInstance(dbDir)
  PropertyTypes.registerAll(store)

  // TODO add an index Step->EntityId for faster lookups

  def transaction[T](f: StoreTransaction => T): T = {
    var result: Option[T] = None
    store.executeInTransaction(new StoreTransactionalExecutable {
      override def execute(txn: StoreTransaction) = { result = Some(f(txn)) }
    })
    result.get
  }

  def readOnlyTransaction[T](f: StoreTransaction => T): T = {
    var result: Option[T] = None
    store.executeInReadonlyTransaction(new StoreTransactionalExecutable {
      override def execute(txn: StoreTransaction) = { result = Some(f(txn)) }
    })
    result.get
  }


  trait EntityObj {
    def id: EntityId
    def entity(txn: StoreTransaction): Entity = txn.getEntity(id)
  }

  class ProofStep(val id: EntityId, val tactic: Tactic[Spec, Goal]) extends GenProofStep[Spec, Goal] with EntityObj {
    def this(id: EntityId, txn: StoreTransaction) =
      this(id, txn.getEntity(id).getProperty(pStepTactic).asInstanceOf[Tactic[Spec, Goal]])
  }


  class Obligation(val id: EntityId, val spec: Spec, val goal: Goal) extends GenObligation[Spec, Goal] with EntityObj {
    def this(id: EntityId, txn: StoreTransaction) =
      this(
        id,
        txn.getEntity(id).getLink(lOblSpec).asInstanceOf[Spec],
        txn.getEntity(id).getProperty(pOblGoal).asInstanceOf[Goal]
      )
  }
  override def newObligation(spec: Spec, goal: Goal) = transaction[Obligation](txn => newObligation(txn, spec, goal))
  def newObligation(txn: StoreTransaction, specObj: Spec, goalObj: Goal): Obligation = {
    val spec = txn.newEntity(TSpec)
    spec.setProperty(pSpecContent, specObj)

    val obl = txn.newEntity(TObligation)
    obl.setProperty(pOblGoal, goalObj)
    obl.setLink(lOblSpec, spec)
    new Obligation(obl.getId, specObj, goalObj)
  }


  /* operations for modifying proof graphs:
   * - add or remove root obligations
   * - apply or unapply a tactic to an obligation, yielding a proof step and subobligations
   * - setting or unsetting the result of validating a proof step
   */

  def addRootObligation(obl: Obligation) = transaction { txn =>
    val root = txn.newEntity(TRoot)
    root.setLink(lRootObl, obl.entity(txn))
  }

  def removeRootObligation(step: Obligation) = ???

  def applyTactic(targetObj: Obligation, tactic: Tactic[Spec, Goal]): ProofStep = {
    val requiredObjs = tactic(this)(targetObj)

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
  def unapplyTactic(obl: Obligation) = ???

  def setVerifiedBy(step: ProofStep, resultObj: StepResult[Spec, Goal]) = transaction { txn =>
    val result = txn.newEntity(TStepResult)
    result.setProperty(pResultStatus, resultObj.status)
    if (resultObj.errorMsg.isDefined)
      result.setProperty(pResultErrorMsg, resultObj.errorMsg.get)
    if (resultObj.evidence.isDefined)
      result.setProperty(pResultEvidence, resultObj.evidence.get)
    step.entity(txn).setLink(lStepVerifiedBy, result)
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

  def rootObligations: Iterable[Obligation] = readOnlyTransaction { txn =>
    txn.getAll(TRoot).asScala.map(root => new Obligation(root.getLink(lRootObl).getId, txn))
  }

  /** Yields proof step if any */
  def appliedStep(obl: Obligation): Option[ProofStep] = ???
  /** Yields required subobligations */
  def requiredObls(step: ProofStep): Iterable[(Obligation, EdgeLabel)] = ???

  /** Yields proof steps that require the given obligation */
  def requiringSteps(obligation: Obligation): Iterable[(ProofStep, EdgeLabel)] = ???
  /** Yields the obligation the proof step was applied to */
  def targetedObl(step: ProofStep): Obligation = ???

  def verifiedBy(step: ProofStep): Option[StepResult[Spec, Goal]] = ???

  /* Traversals */

  /**
    * using a given verifier, verify a given node (assume that every node name is unique for now...)
    * if the node is a leaf, just attempt to verify the node directly via the Solve strategy
    * otherwise, call verifier once for each group of edges that have the same verification strategy as label,
    * passing the corresponding children as assumptions/hypotheses to the verifier
    * @param verifier
    * @param nodename
    * @return updated proof graph, where verification status is correctly propagated along the entire graph
    */
  def verifySingle(verifier: Verifier[Spec, Goal], nodename: String): ProofGraphXodus[Spec, Goal] = {
    // TODO
//    val updatedNode = verifyNode(verifier, nodename)
//    updateNode(nodename, updatedNode.label)
    this
  }

  def computeFullyVerified(nodename: String): Boolean = {
    // TODO
//    val step = graph.context(nodename).label
//    val subgoals = getSubgoalsWithEdges(nodename, graph)
//    if (subgoals.isEmpty)
//      return step.fullyVerified(Seq())
//    val fullyVerifiedStati = subgoals.map {sg => (sg._1, computeFullyVerified(sg._2.vertex))}
//    step.fullyVerified(fullyVerifiedStati.toSeq)
    false
  }

  /**
    * //TODO modify this implementation: take usedEdges from best verification configuration into account for determining which node to call!
    * //also, verifyAll could be lazy: if there is already a finished verification status, don't recompute!
    * try to verify the entire tree
    * @param verifier
    * @return updated proof graph, where verification status is correctly propagated along the entire graph
    */
  def verifyAll(verifier: Verifier[Spec, Goal]): ProofGraphXodus[Spec, Goal] = {
    // TODO
//    val nodes = graph.nodes
//    val verifiedGraph = nodes.foldLeft(graph) { case (g, nodename) =>
//      ProofGraph(g).verifySingle(verifier, nodename).graph
//    }
//    ProofGraph(verifiedGraph)
    this
  }


  /* Modifications */
  //TODO: if graph is mutable, then modifications do not necessarily have to return the updated ProofGraphXodus?
  //TODO: maybe later - improve error handling! if e.g. a from/to node is missing, this currently simply throws a NullPointerException

  /**
    * add a new node to the proof graph
    * @param node
    * @return the new, updated proof graph
    */
  def addProofStep(node: ProofNode[Spec, Goal]): ProofGraphXodus[Spec, Goal] = transaction { txn =>
    val step = txn.newEntity(TProofStep)
//    step.setProperty(pStepName, node.vertex)
    step.setProperty(pOblGoal, node.label.goal)
//    step.setProperty(pStrategy, node.label.tactic)

    // TODO avoid re-adding spec for each proof step
    val spec = txn.newEntity(TSpec)
    spec.setProperty(pSpecContent, node.label.spec)

    step.setLink(lOblSpec, spec)

    this
  }

  def addProofEdge(e: VerificationEdge): ProofGraphXodus[Spec, Goal] = transaction { txn =>
    val edge = txn.newEntity(TEdge)
    edge.setProperty(pEdgeLabel, e.label)

//    val from = txn.find(TProofStep, pStepName, e.from).getFirst
//    val to = txn.find(TProofStep, pStepName, e.to).getFirst

//    from.addLink(lStepRequiredObls, edge)
//    edge.setLink(lEdgeRequiringStep, from)
//    edge.setLink(lEdgeRequiredObl, to)
//    to.addLink(lStepTargetedObl, edge)

    this
  }

  def addProofStep(node: ProofNode[Spec, Goal], edges: Seq[VerificationEdge]): ProofGraphXodus[Spec, Goal] = {
    addProofStep(node)
    edges.foreach(addProofEdge(_))
    this
  }

  /**
    * remove a node from the proof graph
    * @param node
    * @return
    */
  def removeNode(node: ProofNode[Spec, Goal]): ProofGraphXodus[Spec, Goal] = transaction { txn =>
//    for (step <- txn.find(TProofStep, pStepName, node.vertex).asScala) {
//      // when extending the data mode, ensure that a delete does not leave dangling links
//      step.getLink(lOblSpec).delete()
//
//      for (parentEdge <- step.getLinks(lStepTargetedObl).asScala) {
//        val parentStep = parentEdge.getLink(lEdgeRequiringStep)
//        parentStep.deleteLink(lStepRequiredObls, parentEdge)
//        parentEdge.delete()
//      }
//
//      for (kidEdge <- step.getLinks(lStepRequiredObls).asScala) {
//        val kidStep = kidEdge.getLink(lEdgeRequiredObl)
//        kidStep.deleteLink(lStepTargetedObl, kidEdge)
//        kidEdge.delete()
//      }
//
//      step.delete()
//    }
    this
  }


  /* Helpers */

  private def readProofStep(entity: Entity): Obligation = {
    val goal = entity.getProperty(pOblGoal).asInstanceOf[Goal]
    val verificationStrategy = entity.getProperty(pStepTactic).asInstanceOf[Tactic[Spec, Goal]]
    val spec = entity.getLink(lOblSpec).getProperty(pSpecContent).asInstanceOf[Spec]
//    Obligation(spec, goal, verificationStrategy)
    ???
  }
}

object ProofGraphXodus {
  val TRoot = "ROOT"
  val lRootObl = "obl"

  val TObligation = "OBLIGATION"
  val pOblGoal = "goal"
  val lOblSpec = "spec"
  val lOblAppliedStep = "appliedStep"
  val lOblRequiringEdges = "requiringEdges"

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
    registerPropertyType[Solve[String, String]](store)
    registerPropertyType[NoInfoEdgeLabel.type](store)
    registerPropertyType[VerifierStatus[String, String]](store)
    registerPropertyType[Unknown[String, String]](store)
  }

  def registerPropertyType[T <: Comparable[_]](store: PersistentEntityStore)(implicit pickler: SPickler[T], unpickler: Unpickler[T], tag: ClassTag[T], fastTag: FastTypeTag[T]) = store.executeInTransaction(new StoreTransactionalExecutable() {
    override def execute(txn: StoreTransaction): Unit = {
      val format = scala.pickling.binary.pickleFormat

      val binding = new ComparableBinding {
        override def writeObject(output: LightOutputStream, obj: Comparable[_]) = {
          val builder = format.createBuilder(new OutputStreamOutput(output))
          builder.hintTag(fastTag)
          pickler.pickle(obj.asInstanceOf[T], builder)
        }

        override def readObject(stream: ByteArrayInputStream) = {
          val reader = BinaryPickleStream(stream).createReader(fastTag.mirror, format)
          unpickler.unpickle(fastTag, reader).asInstanceOf[T]
        }
      }

      val clazz = tag.runtimeClass.asInstanceOf[Class[_ <: Comparable[_]]]
      store.registerCustomPropertyType(txn, clazz, binding)
    }
  })
}
