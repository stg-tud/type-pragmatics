package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.{ByteArrayInputStream, File}

import de.tu_darmstadt.veritas.VerificationInfrastructure
import jetbrains.exodus.bindings.ComparableBinding
import jetbrains.exodus.entitystore._
import jetbrains.exodus.util.LightOutputStream

import scala.collection.JavaConverters._
import scala.pickling.binary.BinaryPickleStream
import scala.pickling.{FastTypeTag, OutputStreamOutput, SPickler, Unpickler}
import scala.reflect.ClassTag


/**
  * Types: Graph, Step, Edge, Spec
  * Properties:
  *   Spec.content: S
  *   Edge.label: ProofEdgeLabel
  *   Step.name: String
  *   Step.goal: P
  *   Step.verificationStrategy: VerificationStrategy
  * Links:
  *   Step.spec *<->1 Spec._
  *   Graph.root *<->1 Step._
  *   Step.kids 1<->* Edge.from
  *   Step.parents 1<->* Edge.to
  */
class ProofGraphXodus[S <: Comparable[S], P <: Comparable[P]](dbDir: File) {

  val store: PersistentEntityStore = PersistentEntityStores.newInstance(dbDir)
  PropertyTypes.registerAll(store)

  // TODO add an index Step->EntityId for faster lookups


  val TSTEP = "STEP"
  val pStepName = "name"
  val pStepGoal = "goal"
  val pStepVerificationStrategy = "verificationStrategy"
  val lStepSpec = "spec"
  val lStepKids = "kids"
  val lStepParents = "parents"

  val TEDGE = "EDGE"
  val pEdgeLabel = "label"
  val lEdgeFrom = "from"
  val lEdgeTo = "to"

  val TSPEC = "SPEC"
  val pSpecContent = "content"

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


  /* Queries */

  /**
    * Return the ProofStep associated with the given nodename
    * @param nodename
    * @return Is empty if there is no node with nodename otherwise it returns the ProofStep
    */
  def get(nodename: String): Option[ProofStep[S, P]] = readOnlyTransaction { txn =>
    for (step <- txn.find(TSTEP, pStepName, nodename).asScala)
      return Some(readProofStep(step))
    None
  }

  def nodes: List[ProofStep[S, P]] = readOnlyTransaction { txn =>
    val entities = txn.getAll(TSTEP).asScala
    entities.map(readProofStep(_)).toList // copy result to list since the iterable may not leave the transaction
  }


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
  def verifySingle(verifier: Verifier[S, P], nodename: String): ProofGraphXodus[S, P] = {
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
  def verifyAll(verifier: Verifier[S, P]): ProofGraphXodus[S, P] = {
    // TODO
//    val nodes = graph.nodes
//    val verifiedGraph = nodes.foldLeft(graph) { case (g, nodename) =>
//      ProofGraph(g).verifySingle(verifier, nodename).graph
//    }
//    ProofGraph(verifiedGraph)
    this
  }


  /* Modifications */

  /**
    * add a new node to the proof graph
    * @param node
    * @return the new, updated proof graph
    */
  def addNode(node: ProofNode[S, P]): ProofGraphXodus[S, P] = transaction { txn =>
    val step = txn.newEntity(TSTEP)
    step.setProperty(pStepName, node.vertex)
    step.setProperty(pStepGoal, node.label.goal)
    step.setProperty(pStepVerificationStrategy, node.label.verificationStrategy)

    // TODO avoid re-adding spec for each proof step
    val spec = txn.newEntity(TSPEC)
    spec.setProperty(pSpecContent, node.label.spec)

    step.setLink(lStepSpec, spec)

    this
  }

  def addEdge(e: VerificationEdge): ProofGraphXodus[S, P] = transaction { txn =>
    val edge = txn.newEntity(TEDGE)
    edge.setProperty(pEdgeLabel, e.label)

    val from = txn.find(TSTEP, pStepName, e.from).getFirst
    val to = txn.find(TSTEP, pStepName, e.from).getFirst

    from.addLink(lStepKids, edge)
    edge.setLink(lEdgeFrom, from)
    edge.setLink(lEdgeTo, to)
    to.addLink(lStepParents, edge)

    this
  }

  def addNode(node: ProofNode[S, P], edges: Seq[VerificationEdge]): ProofGraphXodus[S, P] = {
    addNode(node)
    edges.foreach(addEdge(_))
    this
  }

  /**
    * remove a node from the proof graph
    * @param node
    * @return
    */
  def removeNode(node: ProofNode[S, P]): ProofGraphXodus[S, P] = transaction { txn =>
    for (step <- txn.find(TSTEP, pStepName, node.vertex).asScala) {
      // when extending the data mode, ensure that a delete does not leave dangling links
      step.getLink(lStepSpec).delete()
      step.delete()
    }
    this
  }


  /* Helpers */

  private def readProofStep(entity: Entity): ProofStep[S, P] = {
    val goal = entity.getProperty(pStepGoal).asInstanceOf[P]
    val verificationStrategy = entity.getProperty(pStepVerificationStrategy).asInstanceOf[VerificationStrategy[S, P]]
    val spec = entity.getLink(lStepSpec).getProperty(pSpecContent).asInstanceOf[S]
    new ProofStep(spec, goal, verificationStrategy)
  }
}


object PropertyTypes {
  def registerAll(store: PersistentEntityStore) {
    registerPropertyType[Solve[String, String]](store)
    registerPropertyType[NoInfoProofEdgeLabel.type](store)
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

