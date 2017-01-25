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

case class Finished[S, P](ps: ProverStatus, usedVerifier: Verifier[S, P]) extends VerificationStatus {
  override val isVerified: Boolean = ps.isVerified
}

case class VerificationFailure[S, P](errorMessage: String, usedVerifier: Verifier[S, P]) extends VerificationStatus

/**
  * type of nodes in a proof graph, represents a single subgoal/step in a proof
  * @param spec the specification from which the goal should be proven
  * @param goal the goal to be proved
  * @tparam S type of the specification format
  * @tparam P type of the format for defining properties
  */
//TODO: maybe put ProofStep class to a separate file later?
class ProofStep[S, P](val spec: S, val goal: P) {
  val verificationStatus: VerificationStatus = NotStarted
  val fullyVerified: Boolean = false

  /**
    * verify the single proof problem:
    * call the given verifier
    * node is only fully verified if all given assumptions are also fully verified
    * @param verifier
    * @param assumptions list of assumptions that shall be used for verification -> typically, all child nodes (connected with the same edge label)
    * @param strat strategy that shall be used for verification -> typically, from edge label
    * @return updated ProofStep with new verification stati
    */
  def verifyNode(verifier: Verifier[S, P],
                 assumptions: Seq[ProofStep[S, P]] = Seq(),
                 strat: VerificationStrategy = Solve): ProofStep[S, P] = {
    val allAssumptions = assumptions map (pp => pp.goal)
    val newverificationStatus = verifier.verify(spec, allAssumptions, goal, strat)
    ProofStep(spec, goal, newverificationStatus).recomputefullyVerified(assumptions)
  }

  /**
    * function for recomputing fullyVerified in a proof step
    * this might be necessary for certain updates in the proof graph, to correctly propagate "fully verified"
    * @param assumptions
    * @return
    */
  //TODO is there a better solution for recomputing verification status than to manually pass the relevant assumptions...?
  def recomputefullyVerified(assumptions: Seq[ProofStep[S, P]] = Seq()): ProofStep[S, P] = {
    val allAssumptionsVerified = assumptions.forall(ps => ps.fullyVerified)
    val newfullyVerified = verificationStatus.isVerified && allAssumptionsVerified
    ProofStep(spec, goal, verificationStatus, newfullyVerified)
  }

  // TODO currently if a child of a child is making current node outdated we get a prev graph which is a nested outdated status, is this desirable?
  // -> Do you have an example? I would like to avoid nested Outdated stati.
  /**
    * mark a proof step as outdated, if there was a previous verification attempt
    * @param pg proof graph before the node became outdated
    * @return
    */
  def makeOutdated(pg: ProofGraph[S, P]): ProofStep[S, P] = {
    verificationStatus match {
      case NotStarted => this //verifications that have not been started can never be Outdated
      case Outdated(prevs, pgold) => this //for the moment, avoid nesting multiple Outdated stati
      case _ => ProofStep(spec, goal, Outdated(verificationStatus, pg))
    }
  }

  def makeFailed(message: String, usedVerifier: Verifier[S, P]): ProofStep[S, P] = {
    ProofStep(spec, goal, VerificationFailure(message, usedVerifier))
  }
}

object ProofStep {

  /*
  public constructor for a ProofStep
   */
  def apply[S, P](spec: S, goal: P): ProofStep[S, P] =
    new ProofStep(spec, goal)

  /*
  private constructor for a ProofStep which may manipulate the verification status
   */
  private def apply[S, P](spec: S, goal: P, nverificationStatus: VerificationStatus, nfullyVerified: Boolean = false): ProofStep[S, P] =
    new ProofStep[S, P](spec, goal) {
      override val verificationStatus = nverificationStatus
      override val fullyVerified: Boolean = nfullyVerified
    }

  def unapply[S, P](arg: ProofStep[S, P]): Option[(S, P)] =
    Some((arg.spec, arg.goal))
}


class ProofGraph[S, P] {

  //idea: other parts of the program should not be able to freely manipulate the internal graph
  //(mostly, because this could yield inconsistent verification stati)
  protected val graph: Graph[String, ProofStep[S, P], VerificationStrategy] = mkGraph(Seq(), Seq())

  //all valid proof graphs have to be acyclic
//  require(!graph.hasLoop)

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
    * change a particular verification edge
    * @param oldedge
    * @param newedge
    * @return
    */
  def updateEdge(oldedge: VerificationEdge, newedge: VerificationEdge): ProofGraph[S, P] = {
    // it is possible that the oldedge and newedge connection completley different node
    // therefore we have to consider the origin of both edges
    val newGraph = graph.removeLEdge(oldedge).safeAddEdge(newedge)
    val parentOld = LNode(oldedge.from, newGraph.label(oldedge.from).get)
    val parentNew = LNode(newedge.from, newGraph.label(newedge.from).get)
    val transitiveParentsOld = getParentNodes(parentOld, graph)
    val transitiveParentsNew = getParentNodes(parentNew, newGraph)
    val mergedTransitiveParents = (transitiveParentsNew ++ transitiveParentsOld).distinct
    makeNodesOutdated(mergedTransitiveParents, newGraph)
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
    val updatedProofstep =
      if (isLeaf) {
        proofstep.verifyNode(verifier)
      } else {
        val subgoalsGrouped = getSubgoalsGroupedByStrategy(nodename)
        val updatedSteps = subgoalsGrouped.map { case (strat, subgoals) =>
            proofstep.verifyNode(verifier, subgoals.toSeq, strat)
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
    val tempGraph = updateNode(nodename, updatedProofstep)
    val transitiveParents = getParentNodes(LNode(focusedGraph.vertex, focusedGraph.label), tempGraph.graph)
    // recompute fully verfied for all parents
    // needs to be done in correct order
    val verifiedParents = transitiveParents.filter { _.label.verificationStatus.isVerified }
    val propagatedGraph = verifiedParents.foldLeft(tempGraph.graph) { case (g, node) =>
        computeFullyVerified(node, g).graph
    }
    ProofGraph(propagatedGraph)
  }

  private def getSubgoalsGroupedByStrategy(nodename: String): Map[VerificationStrategy, Vector[ProofStep[S, P]]] = {
    val context = graph.context(nodename)
    val grouped = context.outEdges.groupBy { _.label }
    grouped.mapValues { edges =>
      edges.map { e =>
        graph.context(e.to).label
      }
    }
  }

  private def containsContradictingStati(stati: Seq[VerificationStatus]): Boolean = {
    val proverStati = stati.foldLeft(Seq.empty[ProverStatus]) { case (l, status) =>
      status match {
        case Finished(ps, _) => Seq(ps) ++ l
        case _ => l
      }
    }
    val containsProved = proverStati.exists {
      case Proved(_, _) => true
      case _ => false
    }
    val containsDisproved = proverStati.exists {
      case Disproved(_, _) => true
      case _ => false
    }
    containsProved && containsDisproved
  }

  private def sortByVerificationStatus(x: ProofStep[S, P], y: ProofStep[S, P]): Boolean = {
    if (x.verificationStatus.isVerified)
      true
    else
      x.verificationStatus match {
        case Finished(Inconclusive, _) => false
        case Finished(Disproved(_, _), _) =>
          !y.verificationStatus.isVerified
        case _ => false
      }
  }

  private def updateNode(nodename: String, newStep: ProofStep[S, P], g: Graph[String, ProofStep[S, P], VerificationStrategy] = graph): ProofGraph[S, P] = {
    val newNode = LNode(nodename, newStep)
    val updatedGraph = g.updateNode(newNode)
    val transitiveParents = getParentNodes(newNode, updatedGraph)
    makeNodesOutdated(transitiveParents, updatedGraph)
  }

  private def computeFullyVerified(node: ProofNode[S, P], g: Graph[String, ProofStep[S, P], VerificationStrategy] = graph): ProofGraph[S, P] = {
    val nodename = node.vertex
    val subgoals = getSubgoalsGroupedByStrategy(nodename).values.flatten.toSeq
    val recomputedStep = node.label.recomputefullyVerified(subgoals)
    updateNode(nodename, recomputedStep)
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

