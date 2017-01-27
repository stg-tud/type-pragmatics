package de.tu_darmstadt.veritas.VerificationInfrastructure

import scala.collection.GenSeq

import quiver._

/**
  * status of a particular verification attempt (for a node/leaf in a proof tree)
  *
  */
sealed trait VerificationStatus {
  val isVerified: Boolean = false
}

case object NotStarted extends VerificationStatus

case class Outdated[S, P](prevs: VerificationStatus, previousProofGraph: ProofGraph[S, P]) extends VerificationStatus

case class Finished[S, P, +V](ps: ProverStatus, t: Transformer[S, P, V], usedVerifier: Verifier[S, P]) extends VerificationStatus {
  override val isVerified: Boolean = ps.isVerified
}

case class VerificationFailure[S, P](errorMessage: String, usedVerifier: Verifier[S, P]) extends VerificationStatus

class ProofGraph[S, P] {

  //idea: other parts of the program should not be able to freely manipulate the internal graph
  //(mostly, because this could yield inconsistent verification stati)
  protected val graph: Graph[String, ProofStep[S, P], VerificationStrategy] = mkGraph(Seq(), Seq())

  //all valid proof graphs have to be acyclic
//  require(!graph.hasLoop)

  /**
    * Return the ProofStep associated with the given nodename
    * @param nodename
    * @return Is empty if there is no node with nodename otherwise it returns the ProofStep
    */
  def get(nodename: String): Option[ProofStep[S, P]] = graph.label(nodename)

  /**
    * add a new node to the proof graph
    * if necessary: Outdate the verification status of the node and of all parents up to the root!
    * @param node
    * @param edges
    * @return the new, updated proof graph
    */
  def addNode(node: ProofNode[S, P], edges: Seq[VerificationEdge]): ProofGraph[S, P] = {
    val updatedGraph = graph.addNode(node).addEdges(edges)
    val transitiveParents = getParentNodes(node, updatedGraph)
    ProofGraph(makeNodesOutdated(transitiveParents, updatedGraph).graph)
  }

  /**
    * collect the transitive hull of parents for a node
    * @param node
    * @return
    */
  private def getParentNodes(
      node: ProofNode[S, P],
      g: Graph[String, ProofStep[S, P], VerificationStrategy] = graph): Vector[ProofNode[S, P]] = {
    val context = g.context(node.vertex)
    if (context.inEdges.isEmpty)
      return Vector.empty
    val parentNodes = context.inEdges.map { in =>
      val context = g.context(in.from)
      LNode(context.vertex, context.label)
    }
    parentNodes ++ parentNodes.flatMap { getParentNodes(_) }
  }

  /**
    * mark all proof steps as outdated which are contained in nodes
    * @param nodes
    * @return
    */
  private def makeNodesOutdated(
      nodes: Vector[ProofNode[S, P]],
      g: Graph[String, ProofStep[S, P], VerificationStrategy] = graph): ProofGraph[S, P] = {
    val stepsToBeOutdated = nodes.map { _.label }
    val outdatedGraph = g.nmap { n =>
      if (stepsToBeOutdated.contains(n))
        n.makeOutdated(ProofGraph(g))
      else
        n
    }
    ProofGraph(outdatedGraph)
  }

  /**
    * remove a node from the proof graph
    * if necessary: Outdate the verification status of the node and of all parents up to the root!
    * @param node
    * @return
    */
  def removeNode(node: ProofNode[S, P]): ProofGraph[S, P] = {
    val transitiveParents = getParentNodes(node)
    val resultGraph = makeNodesOutdated(transitiveParents).graph.removeNode(node.vertex)
    ProofGraph(resultGraph)
  }

  /**
    * change the strategy of a particular verification edge
    * @param edge
    * @param newStrategy
    * @return
    */
  def updateEdge(edge: VerificationEdge, newStrategy: VerificationStrategy): ProofGraph[S, P] = {
    val updatedEdge = LEdge(edge.from, edge.to, newStrategy)
    val updatedGraph = graph.updateEdge(updatedEdge)
    val originNode = LNode(edge.from, updatedGraph.label(edge.from).get)
    val transitiveParents = getParentNodes(originNode, updatedGraph)
    makeNodesOutdated(transitiveParents, updatedGraph)
  }

  /**
    * removes the edge from the graph
    * @param edge
    * @return
    */
  def removeEdge(edge: VerificationEdge): ProofGraph[S, P] = {
    ProofGraph(graph.removeLEdge(edge))
  }

  /**
    * Add a new edge to the graph if the source or the target of the edge does not exist return the previous graph
    * @param edge
    * @return
    */
  def addEdge(edge: VerificationEdge): ProofGraph[S, P] = {
   ProofGraph(graph.safeAddEdge(edge))
  }

  /**
    * using a given verifier, verify a given node (assume that every node name is unique for now...)
    * if the node is a leaf, just attempt to verify the node directly via the Solve strategy
    * otherwise, call verifier once for each group of edges that have the same verification strategy as label,
    * passing the corresponding children as assumptions/hypotheses to the verifier
    * @param verifier
    * @param nodename
    * @return updated proof graph, where verification status is correctly propagated along the entire graph
    */
  def verifySingle(verifier: Verifier[S, P], nodename: String): ProofGraph[S, P] = {
    val focusedGraph = graph.context(nodename)
    val proofstep = focusedGraph.label
    val isLeaf = focusedGraph.outEdges.isEmpty
    val updatedNode = verifyNode(verifier, LNode(nodename, proofstep))
    val updatedGraph = updateNode(nodename, updatedNode.label)
    val transitiveParents = getParentNodes(LNode(focusedGraph.vertex, focusedGraph.label), updatedGraph.graph)
    // recompute fully verfied for all parents
    // needs to be done in correct order
    val verifiedParents = transitiveParents.filter { _.label.verificationStatus.isVerified }
    val propagatedGraph = verifiedParents.foldLeft(updatedGraph.graph) { case (g, node) =>
        computeFullyVerified(node, g).graph
    }
    ProofGraph(propagatedGraph)
  }

  private def verifyNode(verifier: Verifier[S, P], node: ProofNode[S, P]): ProofNode[S, P] = {
    val focusedGraph = graph.context(node.vertex)
    val proofstep = node.label
    val isLeaf = focusedGraph.outEdges.isEmpty
    val updatedStep =
      if (isLeaf) {
        proofstep.verify(verifier)
      } else {
        val subgoalsGrouped = getSubgoalsGroupedByStrategy(node.vertex)
        val updatedSteps = subgoalsGrouped.map { case (strat, subgoals) =>
          proofstep.verify(verifier, subgoals.toSeq, strat)
        }.toSeq
        val stati = updatedSteps.map { _.verificationStatus }
        val contradictingStati = containsContradictingStati(stati)
        if (contradictingStati) {
          proofstep.makeFailed("Verifier found a prove and a contradiction at the same time.", verifier)
        } else {
          // order is proved < disproved < inconclusive
          val sortedSteps = updatedSteps.sortWith(sortByVerificationStatus)
          // we can assume that at least one element exists because node is not a leave
          sortedSteps(0)
        }
      }
    LNode(node.vertex, updatedStep)
  }

  private def getSubgoalsGroupedByStrategy(nodename: String, g: Graph[String, ProofStep[S, P], VerificationStrategy] = graph): Map[VerificationStrategy, Vector[ProofStep[S, P]]] = {
    val context = g.context(nodename)
    val grouped = context.outEdges.groupBy { _.label }
    grouped.mapValues { edges =>
      edges.map { e =>
        g.context(e.to).label
      }
    }
  }

  private def containsContradictingStati(stati: Seq[VerificationStatus]): Boolean = {
    val proverStati = stati.foldLeft(Seq.empty[ProverStatus]) { case (l, status) =>
      status match {
        case Finished(ps, _, _) => Seq(ps) ++ l
        case _ => l
      }
    }
    val containsProved = proverStati.exists {
      case Proved(_) => true
      case _ => false
    }
    val containsDisproved = proverStati.exists {
      case Disproved(_) => true
      case _ => false
    }
    containsProved && containsDisproved
  }

  private def sortByVerificationStatus(x: ProofStep[S, P], y: ProofStep[S, P]): Boolean = {
    if (x.verificationStatus.isVerified)
      true
    else
      x.verificationStatus match {
        case Finished(Inconclusive, _, _) => false
        case Finished(Disproved(_), _, _) =>
          !y.verificationStatus.isVerified
        case _ => false
      }
  }

  private def updateNode(nodename: String, newStep: ProofStep[S, P], g: Graph[String, ProofStep[S, P], VerificationStrategy] = graph): ProofGraph[S, P] = {
    val newNode = LNode(nodename, newStep)
    val updatedGraph = g.updateNode(newNode)
    ProofGraph(updatedGraph)
  }

  private def computeFullyVerified(node: ProofNode[S, P], g: Graph[String, ProofStep[S, P], VerificationStrategy] = graph): ProofGraph[S, P] = {
    val nodename = node.vertex
    val subgoals = getSubgoalsGroupedByStrategy(nodename, g).values.flatten.toSeq
    val recomputedStep = node.label.recomputefullyVerified(subgoals)
    updateNode(nodename, recomputedStep, g)
  }

  /**
    * try to verify the entire tree
    * @param verifier
    * @return updated proof graph, where verification status is correctly propagated along the entire graph
    */
  def verifyAll(verifier: Verifier[S, P]): ProofGraph[S, P] = {
    val roots = graph.roots.toSeq
    // verification order is children before parents to correctly compute fullyVerified
    val nodes = graph.bfsn(roots).reverse
    val verifiedGraph = nodes.foldLeft(graph) { case (g, nodename) =>
        ProofGraph(g).verifySingle(verifier, nodename).graph
    }
    ProofGraph(verifiedGraph)
  }

  def verifyAllPar(verifier: Verifier[S, P]): ProofGraph[S, P] = {
    val contexts = graph.contexts.par
    val verifiedContexts = contexts.map { context =>
      val updatedNode = verifyNode(verifier, LNode(context.vertex, context.label))
      Context(context.inAdj, updatedNode.vertex, updatedNode.label, context.outAdj)
    }
    val verifiedGraph =
      verifiedContexts.seq.foldLeft(empty[String, ProofStep[S, P], VerificationStrategy]) {
        case (result, context) => result & context
      }
    ProofGraph(verifiedGraph).recomputeFullyVerified()
  }

  private def recomputeFullyVerified(): ProofGraph[S, P] = {
    val roots = graph.roots.toSeq
    // verification order is children before parents to correctly compute fullyVerified
    val nodenames = graph.bfsn(roots).reverse
    val propagatedGraph = nodenames.foldLeft(graph) { case (result, nodename) =>
      val node = LNode(nodename, result.label(nodename).get)
      computeFullyVerified(node, result).graph
    }
    ProofGraph(propagatedGraph)
  }

  //TODO add functions for "pretty printing" the graph: simple ones that construct a string,
  // maybe functions that pretty print single nodes, etc.
  // - if possible, a function that generates a graph via graphviz (maybe it is possible to specify different colors
  //for different verification stati...? etc.



}

object ProofGraph {

  /**
    * public constructor
    */
  def apply[S, P](nodelist: Seq[ProofNode[S, P]], edgelist: Seq[VerificationEdge]): ProofGraph[S, P] =
    new ProofGraph[S, P] {
      override val graph = safeMkGraph(nodelist, edgelist) //ignores dangling edges
    }

  /**
    * private constructor for directly generating a new ProofGraph instance without having to reconstruct the graph
    */
  private def apply[S, P](newgraph: Graph[String, ProofStep[S, P], VerificationStrategy]): ProofGraph[S, P] =
    new ProofGraph[S, P] {
      override val graph = newgraph
    }

}

