package de.tu_darmstadt.veritas.VerificationInfrastructure

import scala.collection.GenSeq

import quiver._


//class ProofGraph[S, P] {
//
//  //idea: other parts of the program should not be able to freely manipulate the internal graph
//  //(mostly, because this could yield inconsistent verification stati)
//  protected val graph: Graph[String, ProofStep[S, P], VerificationStrategy] = mkGraph(Seq(), Seq())
//
//  //all valid proof graphs have to be acyclic
////  require(!graph.hasLoop)
//
//  /**
//    * Return the ProofStep associated with the given nodename
//    * @param nodename
//    * @return Is empty if there is no node with nodename otherwise it returns the ProofStep
//    */
//  def get(nodename: String): Option[ProofStep[S, P]] = graph.label(nodename)
//
//  /**
//    * add a new node to the proof graph
//    * if necessary: Outdate the verification status of the node and of all parents up to the root!
//    * @param node
//    * @param edges
//    * @return the new, updated proof graph
//    */
//  def addNode(node: ProofNode[S, P], edges: Seq[VerificationEdge]): ProofGraph[S, P] = {
//    val updatedGraph = graph.addNode(node).addEdges(edges)
//    val transitiveParents = getParentPaths(node, updatedGraph)
//    ProofGraph(makeNodesOutdated(transitiveParents, updatedGraph).graph)
//  }
//
//  /**
//    * collect the transitive hull of parents for a node
//    * @param node
//    * @return
//    */
//  private def getParentPaths(
//      node: ProofNode[S, P],
//      g: Graph[String, ProofStep[S, P], VerificationStrategy] = graph): Vector[(ProofNode[S, P], VerificationStrategy)] = {
//    val context = g.context(node.vertex)
//    if (context.inEdges.isEmpty)
//      return Vector.empty
//    val parentPaths = context.inEdges.map { in =>
//      val context = g.context(in.from)
//      (LNode(context.vertex, context.label), in.label)
//    }
//    parentPaths ++ parentPaths.flatMap { path => getParentPaths(path._1) }
//  }
//
//  /**
//    * mark all proof steps as outdated which are contained in nodes
//    * @param nodes
//    * @return
//    */
//  private def makeNodesOutdated(
//      nodes: Vector[(ProofNode[S, P], VerificationStrategy)],
//      g: Graph[String, ProofStep[S, P], VerificationStrategy] = graph): ProofGraph[S, P] = {
//    println(nodes)
//    val outdatedGraph = nodes.foldLeft(g) { case (newGraph, (node, strat)) =>
//        val outdatedStep = node.label.makeOutdated(ProofGraph(newGraph), strat)
//        val outdatedNode = LNode(node.vertex, outdatedStep)
//        newGraph.updateNode(outdatedNode)
//    }
//    ProofGraph(outdatedGraph)
//  }
//
//  /**
//    * remove a node from the proof graph
//    * if necessary: Outdate the verification status of the node and of all parents up to the root!
//    * @param node
//    * @return
//    */
//  def removeNode(node: ProofNode[S, P]): ProofGraph[S, P] = {
//    val transitiveParents = getParentPaths(node)
//    val resultGraph = makeNodesOutdated(transitiveParents).graph.removeNode(node.vertex)
//    ProofGraph(resultGraph)
//  }
//
//  /**
//    * change the strategy of a particular verification edge
//    * @param edge
//    * @param newStrategy
//    * @return
//    */
//  def updateEdge(edge: VerificationEdge, newStrategy: VerificationStrategy): ProofGraph[S, P] = {
//    val updatedEdge = LEdge(edge.from, edge.to, newStrategy)
//    val updatedGraph = graph.updateEdge(updatedEdge)
//    val originNode = LNode(edge.from, updatedGraph.label(edge.from).get)
//    val transitiveParents = getParentPaths(originNode, updatedGraph)
//    makeNodesOutdated(transitiveParents, updatedGraph)
//  }
//
//  /**
//    * removes the edge from the graph
//    * @param edge
//    * @return
//    */
//  def removeEdge(edge: VerificationEdge): ProofGraph[S, P] = {
//    ProofGraph(graph.removeLEdge(edge))
//  }
//
//  /**
//    * Add a new edge to the graph if the source or the target of the edge does not exist return the previous graph
//    * @param edge
//    * @return
//    */
//  def addEdge(edge: VerificationEdge): ProofGraph[S, P] = {
//   ProofGraph(graph.safeAddEdge(edge))
//  }
//
//  /**
//    * using a given verifier, verify a given node (assume that every node name is unique for now...)
//    * if the node is a leaf, just attempt to verify the node directly via the Solve strategy
//    * otherwise, call verifier once for each group of edges that have the same verification strategy as label,
//    * passing the corresponding children as assumptions/hypotheses to the verifier
//    * @param verifier
//    * @param nodename
//    * @return updated proof graph, where verification status is correctly propagated along the entire graph
//    */
//  def verifySingle(verifier: Verifier[S, P], nodename: String): ProofGraph[S, P] = {
//    val focusedGraph = graph.context(nodename)
//    val proofstep = focusedGraph.label
//    val isLeaf = focusedGraph.outEdges.isEmpty
//    val updatedNode = verifyNode(verifier, LNode(nodename, proofstep))
//    val updatedGraph = updateNode(nodename, updatedNode.label)
//    val transitiveParents = getParentPaths(LNode(focusedGraph.vertex, focusedGraph.label), updatedGraph.graph)
//    // recompute fully verfied for all parents
//    // needs to be done in correct order
//    val verifiedParents = transitiveParents.filter { _._1.label.verificationStatus.isVerified }
//    val propagatedGraph = verifiedParents.foldLeft(updatedGraph.graph) { case (g, node) =>
//        computeFullyVerified(node._1, g).graph
//    }
//    ProofGraph(propagatedGraph)
//  }
//
//  private def verifyNode(verifier: Verifier[S, P], node: ProofNode[S, P]): ProofNode[S, P] = {
//    val focusedGraph = graph.context(node.vertex)
//    val proofstep = node.label
//    val isLeaf = focusedGraph.outEdges.isEmpty
//    val updatedStep =
//      if (isLeaf) {
//        proofstep.verify(verifier)
//      } else {
//        val subgoalsGrouped = getSubgoalsGroupedByStrategy(node.vertex)
//        subgoalsGrouped.foldLeft(proofstep) { case (step, (strategy, assumptions)) =>
//            step.verify(verifier, assumptions, strategy)
//        }
//      }
//    LNode(node.vertex, updatedStep)
//  }
//
//  private def getSubgoalsGroupedByStrategy(nodename: String, g: Graph[String, ProofStep[S, P], VerificationStrategy] = graph): Map[VerificationStrategy, Vector[ProofStep[S, P]]] = {
//    val context = g.context(nodename)
//    val grouped = context.outEdges.groupBy { _.label }
//    grouped.mapValues { edges =>
//      edges.map { e =>
//        g.context(e.to).label
//      }
//    }
//  }
//
//  private def updateNode(nodename: String, newStep: ProofStep[S, P], g: Graph[String, ProofStep[S, P], VerificationStrategy] = graph): ProofGraph[S, P] = {
//    val newNode = LNode(nodename, newStep)
//    val updatedGraph = g.updateNode(newNode)
//    ProofGraph(updatedGraph)
//  }
//
//  private def computeFullyVerified(node: ProofNode[S, P], g: Graph[String, ProofStep[S, P], VerificationStrategy] = graph): ProofGraph[S, P] = {
//    val nodename = node.vertex
//    val subgoalsGrouped = getSubgoalsGroupedByStrategy(nodename, g)
//    val recomputedStep = subgoalsGrouped.foldLeft(node.label) { case (step, (strat, assumptions)) =>
//        step.recomputefullyVerified(assumptions, strat)
//    }
//    updateNode(nodename, recomputedStep, g)
//  }
//
//  /**
//    * try to verify the entire tree
//    * @param verifier
//    * @return updated proof graph, where verification status is correctly propagated along the entire graph
//    */
//  def verifyAll(verifier: Verifier[S, P]): ProofGraph[S, P] = {
//    val roots = graph.roots.toSeq
//    // verification order is children before parents to correctly compute fullyVerified
//    val nodes = graph.bfsn(roots).reverse
//    val verifiedGraph = nodes.foldLeft(graph) { case (g, nodename) =>
//        ProofGraph(g).verifySingle(verifier, nodename).graph
//    }
//    ProofGraph(verifiedGraph)
//  }
//
//  def verifyAllPar(verifier: Verifier[S, P]): ProofGraph[S, P] = {
//    val contexts = graph.contexts.par
//    val verifiedContexts = contexts.map { context =>
//      val updatedNode = verifyNode(verifier, LNode(context.vertex, context.label))
//      Context(context.inAdj, updatedNode.vertex, updatedNode.label, context.outAdj)
//    }
//    val verifiedGraph =
//      verifiedContexts.seq.foldLeft(empty[String, ProofStep[S, P], VerificationStrategy]) {
//        case (result, context) => result & context
//      }
//    ProofGraph(verifiedGraph).recomputeFullyVerified()
//  }
//
//  private def recomputeFullyVerified(): ProofGraph[S, P] = {
//    val roots = graph.roots.toSeq
//    // verification order is children before parents to correctly compute fullyVerified
//    val nodenames = graph.bfsn(roots).reverse
//    val propagatedGraph = nodenames.foldLeft(graph) { case (result, nodename) =>
//      val node = LNode(nodename, result.label(nodename).get)
//      computeFullyVerified(node, result).graph
//    }
//    ProofGraph(propagatedGraph)
//  }
//
//  //TODO add functions for "pretty printing" the graph: simple ones that construct a string,
//  // maybe functions that pretty print single nodes, etc.
//  // - if possible, a function that generates a graph via graphviz (maybe it is possible to specify different colors
//  //for different verification stati...? etc.
//
//
//
//}
//
//object ProofGraph {
//
//  /**
//    * public constructor
//    */
//  def apply[S, P](nodelist: Seq[ProofNode[S, P]], edgelist: Seq[VerificationEdge]): ProofGraph[S, P] =
//    new ProofGraph[S, P] {
//      override val graph = safeMkGraph(nodelist, edgelist) //ignores dangling edges
//    }
//
//  /**
//    * private constructor for directly generating a new ProofGraph instance without having to reconstruct the graph
//    */
//  private def apply[S, P](newgraph: Graph[String, ProofStep[S, P], VerificationStrategy]): ProofGraph[S, P] =
//    new ProofGraph[S, P] {
//      override val graph = newgraph
//    }
//
//}
//
