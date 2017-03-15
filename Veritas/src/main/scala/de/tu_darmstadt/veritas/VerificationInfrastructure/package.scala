package de.tu_darmstadt.veritas

import quiver.{Context, Graph, LEdge, LNode}

/**
  * Created by sylvia on 19/01/17.
  * collect a couple of type aliases
  */
package object VerificationInfrastructure {

  type ProofNode[S, P] = LNode[String, Obligation[S, P]]
  type VerificationEdge = LEdge[String, EdgeLabel]
  type InternalProofGraph[S, P] = Graph[String, Obligation[S, P], EdgeLabel]

  type FocusedExpDomTree[S, V] = Context[String, ExpressionDomainNode[S, V], DomainEdgeLabel]
  type FocusedTypDomTree[S, V] = Context[String, TypeDomainNode[S, V], DomainEdgeLabel]

}
