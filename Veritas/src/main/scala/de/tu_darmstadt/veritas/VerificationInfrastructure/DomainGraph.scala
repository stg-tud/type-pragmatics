package de.tu_darmstadt.veritas.VerificationInfrastructure

import quiver.{Context, Graph, LEdge, LNode, mkGraph, safeMkGraph}

/**
  * Created by sylvia on 27/02/2017.
  */
class DomainGraph[C, I] {
  type Domain <: DomainNode[C, I]
  type DomainInternal = LNode[String, Domain]
  type DomainEdge <: DomainEdgeLabel
  type DomainEdgeInternal = LEdge[String, DomainEdge]

  type InternalDomainTree = Graph[String, Domain, DomainEdge]
  type FocusedTree = Context[String, Domain, DomainEdge]

  protected val graph: InternalDomainTree = mkGraph(Seq(), Seq())
}

object DomainGraph {
  /**
    * public constructor
    *
    * @param nodelist
    * @param edgelist
    */
  def apply[C, I, D <: DomainNode[C, I], E <: DomainEdgeLabel]
  (nodelist: Seq[LNode[String, D]], edgelist: Seq[LEdge[String, E]]): DomainGraph[C, I] = new DomainGraph[C, I] {
    type Domain = D
    type DomainEdge = E

    override protected val graph: InternalDomainTree = safeMkGraph(nodelist, edgelist)
  }

  /**
    * private constructor for directly generating a new DomainGraph instance without having to reconstruct the graph
    */
  private def apply[C, I, D <: DomainNode[C, I], E <: DomainEdgeLabel]
  (newgraph: Graph[String, D, E]): DomainGraph[C, I] = new DomainGraph[C, I] {
    type Domain = D
    type DomainEdge = E

    override protected val graph: InternalDomainTree = newgraph
  }
}


//special graph instances are probably not needed!
//class ExpressionDomainGraph[C, I] extends DomainGraph[C, I] {
//  override type Domain = ExpressionDomainNode[C, I]
//  override type DomainEdge = ExpressionDomainEdgeLabel
//
//}
//
//class TypeDomainGraph[C, I] extends DomainGraph[C, I] {
//  override type Domain = TypeDomainNode[C, I]
//  override type DomainEdge = DomainEdgeLabel //TODO: refine expression domain edge?
//
//}
//
//class DynamicDomainGraph[C, I, P] extends DomainGraph[C, I] {
//  override type Domain = DynamicDomainNode[C, I, P]
//  override type DomainEdge = DomainEdgeLabel //TODO: refine expression domain edge?
//
//}
//
//class StaticDomainGraph[C, I, P] extends DomainGraph[C, I] {
//  override type Domain = StaticDomainNode[C, I, P]
//  override type DomainEdge = DomainEdgeLabel //TODO: refine expression domain edge?
//
//}
