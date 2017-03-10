package de.tu_darmstadt.veritas.VerificationInfrastructure

import quiver._

class ProofGraphQuiver[S, P] {

  //idea: other parts of the program should not be able to freely manipulate the internal graph
  //(mostly, because this could yield inconsistent verification stati)
  protected val graph: InternalProofGraph[S, P] = mkGraph(Seq(), Seq())

  //all valid proof graphs have to be acyclic
  // Why comment this? Does it not work?
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
  def addNode(node: ProofNode[S, P], edges: Seq[VerificationEdge]): ProofGraphQuiver[S, P] = {
    val updatedGraph = graph.addNode(node).addEdges(edges)
    val transitiveParents = getParentPaths(node, updatedGraph)
    ProofGraphQuiver(makeNodesOutdated(transitiveParents, updatedGraph).graph)
  }

  //TODO: add a method that allows for replacing an entire subgraph with a different subgraph

  /**
    * collect the transitive hull of parents for a node
    * @param node
    * @return
    */
  private def getParentPaths(
      node: ProofNode[S, P],
      g: InternalProofGraph[S, P] = graph): Vector[(ProofEdgeLabel, ProofNode[S, P])] = {
    val context = g.context(node.vertex)
    if (context.inEdges.isEmpty)
      return Vector.empty
    val parentPaths = context.inEdges.map { in =>
      val context = g.context(in.from)
      (in.label, LNode(context.vertex, context.label))
    }
    parentPaths ++ parentPaths.flatMap { path => getParentPaths(path._2) }
  }

  /**
    * mark all proof steps as outdated which are contained in nodes
    * @param nodes
    * @return
    */
  private def makeNodesOutdated(
                                 nodes: Vector[(ProofEdgeLabel, ProofNode[S, P])],
                                 g: InternalProofGraph[S, P] = graph): ProofGraphQuiver[S, P] = {
    val outdatedGraph = nodes.foldLeft(g) { case (newGraph, (edgeinfo, node)) =>
        val outdatedStep = node.label.makeOutdated(ProofGraphQuiver(newGraph))
        val outdatedNode = LNode(node.vertex, outdatedStep)
        newGraph.updateNode(outdatedNode)
    }
    ProofGraphQuiver(outdatedGraph)
  }

  /**
    * remove a node from the proof graph
    * if necessary: Outdate the verification status of the node and of all parents up to the root!
    * @param node
    * @return
    */
  def removeNode(node: ProofNode[S, P]): ProofGraphQuiver[S, P] = {
    val transitiveParents = getParentPaths(node)
    val resultGraph = makeNodesOutdated(transitiveParents).graph.removeNode(node.vertex)
    ProofGraphQuiver(resultGraph)
  }

  /**
    * change the strategy of a particular verification edge
    * @param edge
    * @param newEdgeLabel
    * @return
    */
  def updateEdge(edge: VerificationEdge, newEdgeLabel: ProofEdgeLabel): ProofGraphQuiver[S, P] = {
    val updatedEdge = LEdge(edge.from, edge.to, newEdgeLabel)
    val updatedGraph = graph.updateEdge(updatedEdge)
    val originNode = LNode(edge.from, updatedGraph.label(edge.from).get)
    val transitiveParents = getParentPaths(originNode, updatedGraph)
    makeNodesOutdated(transitiveParents, updatedGraph)
  }

  /**
    * removes the edge from the graph
    * @param edge
    * @return
    */
  def removeEdge(edge: VerificationEdge): ProofGraphQuiver[S, P] = {
    ProofGraphQuiver(graph.removeLEdge(edge))
  }

  /**
    * Add a new edge to the graph if the source or the target of the edge does not exist return the previous graph
    * @param edge
    * @return
    */
  def addEdge(edge: VerificationEdge): ProofGraphQuiver[S, P] = {
   ProofGraphQuiver(graph.safeAddEdge(edge))
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
  def verifySingle(verifier: Verifier[S, P], nodename: String): ProofGraphQuiver[S, P] = {
    val updatedNode = verifyNode(verifier, nodename)
    updateNode(nodename, updatedNode.label)
  }

  private def verifyNode(verifier: Verifier[S, P], nodename: String): ProofNode[S, P] = {
    val focusedGraph = graph.context(nodename)
    val step = focusedGraph.label
    val isLeaf = focusedGraph.outEdges.isEmpty
    val updatedStep =
      if (isLeaf) {
        step.verify(verifier)
      } else {
        val assumptions = getSubgoalsWithEdges(nodename).map { sg => (sg._1, sg._2.label)}
        step.verify(verifier, assumptions)
      }
    LNode(nodename, updatedStep)
  }

  private def getSubgoalsWithEdges(nodename: String, g: InternalProofGraph[S, P] = graph): Vector[(ProofEdgeLabel, ProofNode[S, P])] = {
    val context = g.context(nodename)
    val edges = context.outEdges
    edges.map { e =>
      (e.label, LNode(e.to, g.context(e.to).label))
    }
  }

  private def updateNode(nodename: String, newStep: ProofStep[S, P], g: InternalProofGraph[S, P] = graph): ProofGraphQuiver[S, P] = {
    val newNode = LNode(nodename, newStep)
    val updatedGraph = g.updateNode(newNode)
    ProofGraphQuiver(updatedGraph)
  }

  def computeFullyVerified(nodename: String): Boolean = {
    val step = graph.context(nodename).label
    val subgoals = getSubgoalsWithEdges(nodename, graph)
    if (subgoals.isEmpty)
      return step.fullyVerified(Seq())
    val fullyVerifiedStati = subgoals.map {sg => (sg._1, computeFullyVerified(sg._2.vertex))}
    step.fullyVerified(fullyVerifiedStati.toSeq)
  }

  /**
    * //TODO modify this implementation: take usedEdges from best verification configuration into account for determining which node to call!
    * //also, verifyAll could be lazy: if there is already a finished verification status, don't recompute!
    * try to verify the entire tree
    * @param verifier
    * @return updated proof graph, where verification status is correctly propagated along the entire graph
    */
  def verifyAll(verifier: Verifier[S, P]): ProofGraphQuiver[S, P] = {
    val nodes = graph.nodes
    val verifiedGraph = nodes.foldLeft(graph) { case (g, nodename) =>
        ProofGraphQuiver(g).verifySingle(verifier, nodename).graph
    }
    ProofGraphQuiver(verifiedGraph)
  }

  def verifyAllPar(verifier: Verifier[S, P]): ProofGraphQuiver[S, P] = {
    val contexts = graph.contexts.par
    val verifiedContexts = contexts.map { context =>
      val updatedNode = verifyNode(verifier, context.vertex)
      Context(context.inAdj, updatedNode.vertex, updatedNode.label, context.outAdj)
    }
    val verifiedGraph =
      verifiedContexts.seq.foldLeft(empty[String, ProofStep[S, P], ProofEdgeLabel]) {
        case (result, context) => result & context
      }
    ProofGraphQuiver(verifiedGraph)
  }


  //TODO add functions for "pretty printing" the graph: simple ones that construct a string,
  // maybe functions that pretty print single nodes, etc.
  // - if possible, a function that generates a graph via graphviz (maybe it is possible to specify different colors
  //for different verification stati...? etc.



}

object ProofGraphQuiver {

  /**
    * public constructor
    */
  def apply[S, P](nodelist: Seq[ProofNode[S, P]], edgelist: Seq[VerificationEdge] = Seq()): ProofGraphQuiver[S, P] =
    new ProofGraphQuiver[S, P] {
      override val graph = safeMkGraph(nodelist, edgelist) //ignores dangling edges
    }

  /**
    * private constructor for directly generating a new ProofGraph instance without having to reconstruct the graph
    */
  private def apply[S, P](newgraph: InternalProofGraph[S, P]): ProofGraphQuiver[S, P] =
    new ProofGraphQuiver[S, P] {
      override val graph = newgraph
    }

}

