package de.tu_darmstadt.veritas.VerificationInfrastructure

import quiver.{Context, Graph, LEdge, LNode, mkGraph, safeMkGraph}

/**
  * Created by sylvia on 27/02/2017.
  */
abstract class DomainGraph[S, V] {
  type Domain <: DomainNode[S, V]
  //type DomainInternal[S, V] = LNode[String, Domain[S, V]]
  type DomainEdge <: DomainEdgeLabel
  //type DomainEdgeInternal = LEdge[String, DomainEdge]

  type InternalDomainTree = Graph[String, Domain, DomainEdge]
  type FocusedTree = Context[String, Domain, DomainEdge]

  protected val graph: InternalDomainTree = mkGraph(Seq(), Seq())
}


class ExpressionDomainGraph[S, V] extends DomainGraph[S, V] {
  override type Domain = ExpressionDomainNode[S, V]
  override type DomainEdge = DomainEdgeLabel //TODO: refine expression domain edge?

}

class TypeDomainGraph[S, V] extends DomainGraph[S, V] {
  override type Domain = TypeDomainNode[S, V]
  override type DomainEdge = DomainEdgeLabel //TODO: refine expression domain edge?

}

class DynamicDomainGraph[S, V, P] extends DomainGraph[S, V] {
  override type Domain = DynamicDomainNode[S, V, P]
  override type DomainEdge = DomainEdgeLabel //TODO: refine expression domain edge?

}

class StaticDomainGraph[S, V, P] extends DomainGraph[S, V] {
  override type Domain = StaticDomainNode[S, V, P]
  override type DomainEdge = DomainEdgeLabel //TODO: refine expression domain edge?

}
