package de.tu_darmstadt.veritas.VerificationInfrastructure

import quiver.{Context, Graph, LEdge, LNode, mkGraph, safeMkGraph}

/**
  * Created by sylvia on 27/02/2017.
  */
class DomainGraph[C, I] {
  self =>
  type Domain <: DomainNode[C, I]
  //type DomainInternal = LNode[String, Domain]
  type DomainEdge <: DomainEdgeLabel
  //type DomainEdgeInternal = LEdge[String, DomainEdge]

  //type InternalDomainTree = Graph[String, Domain, DomainEdge]
  //type FocusedTree = Context[String, Domain, DomainEdge]

  protected val graph: Graph[String, Domain, DomainEdge] = mkGraph(Seq(), Seq())

   def getInternalGraph: Graph[String, Domain, DomainEdge] = graph

  /**
    * Completely adds the given subgraph to the current graph.
    * Will not duplicate existing nodes, but replace labels of existing nodes with labels from newsubgraph.
    * Will keep all existing edges, and add the edges from newsubgraph
    * (possibly creating multiple edges between two nodes)!
    *
    * Doesn't do anything if the new graph does not share any nodes with the given graph (avoids creating forests!)
    *
    * @param newsubgraph
    * @return
    */
  def addSubgraph(newsubgraph: DomainGraph[C, I] {type Domain = self.Domain; type DomainEdge = self.DomainEdge}): DomainGraph[C, I] {type Domain = self.Domain; type DomainEdge = self.DomainEdge} =
    if ((newsubgraph.getInternalGraph.nodes.toSet intersect graph.nodes.toSet).nonEmpty)
      DomainGraph[C, I, self.Domain, self.DomainEdge](graph.union(newsubgraph.getInternalGraph))
    else {
      println("WARNING: Given subgraph " + newsubgraph + " was not added to the current graph because there was no common node!")
      this
    }

  /**
    * alias for addSubgraph
    *
    * @param newsubgraph
    * @return
    */
  def +++(newsubgraph: DomainGraph[C, I] {type Domain = self.Domain; type DomainEdge = self.DomainEdge}): DomainGraph[C, I] = addSubgraph(newsubgraph)

  /**
    * Completely replaces nodes and edges from the current graph with the nodes/edges from the given graph
    * new nodes and edges from the given graph will also be added
    *
    * Doesn't do anything if the new graph does not share any nodes with the given graph (avoids creating forests!)
    *
    * @param newsubgraph
    * @return
    */
  def replaceSubgraph(newsubgraph: this.type): DomainGraph[C, I] =
    if ((newsubgraph.getInternalGraph.nodes.toSet intersect graph.nodes.toSet).nonEmpty) {
      val newinnergraph = newsubgraph.getInternalGraph
      val (existingnodes, newnodes) = newinnergraph.labNodes.partition(ln =>
        graph.nodes contains ln.vertex)
      val (existingedges, newedges) = newinnergraph.labEdges.partition(le =>
        graph.edges contains le.edge)

      val newgraph = (((graph updateNodes existingnodes) addNodes newnodes) updateEdges
        existingedges) addEdges newedges

      DomainGraph(newgraph)
    } else {
      println("WARNING: Given subgraph " + newsubgraph + " was not replaced in current graph because there was no common node!")
      this
    }

  /**
    * alias for replaceSubgraph
    *
    * @param newsubgraph
    * @return
    */
  def >>>(newsubgraph: this.type): DomainGraph[C, I] = replaceSubgraph(newsubgraph)

}

object DomainGraph {
  /**
    * public constructor
    *
    * @param nodelist
    * @param edgelist
    */
  def apply[C, I, D <: DomainNode[C, I], E <: DomainEdgeLabel]
  (nodelist: Seq[LNode[String, D]], edgelist: Seq[LEdge[String, E]]): DomainGraph[C, I] {type Domain = D; type DomainEdge = E} =
    new DomainGraph[C, I] {
      type Domain = D
      type DomainEdge = E

      override protected val graph: Graph[String, Domain, DomainEdge] = safeMkGraph(nodelist, edgelist)
    }

  /**
    * private constructor for directly generating a new DomainGraph instance without having to reconstruct the graph
    */
  private def apply[C, I, D <: DomainNode[C, I], E <: DomainEdgeLabel]
  (newgraph: Graph[String, D, E]): DomainGraph[C, I] {type Domain = D; type DomainEdge = E} =
    new DomainGraph[C, I] {
      type Domain = D
      type DomainEdge = E

      override protected val graph: Graph[String, Domain, DomainEdge] = newgraph
    }
}
