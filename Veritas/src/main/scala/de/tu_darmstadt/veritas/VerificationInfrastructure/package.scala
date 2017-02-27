package de.tu_darmstadt.veritas

import quiver.{Context, Graph, LEdge, LNode}

/**
  * Created by sylvia on 19/01/17.
  * collect a couple of type aliases
  */
package object VerificationInfrastructure {

  type ProofNode[S, P] = LNode[String, ProofStep[S, P]]
  type VerificationEdge = LEdge[String, ProofEdgeLabel]
  type InternalProofGraph[S, P] = Graph[String, ProofStep[S, P], ProofEdgeLabel]

  type FocusedExpDomTree[S, V] = Context[String, ExpressionDomainNode[S, V], DomainEdgeLabel]
  type FocusedTypDomTree[S, V] = Context[String, TypeDomainNode[S, V], DomainEdgeLabel]

}
