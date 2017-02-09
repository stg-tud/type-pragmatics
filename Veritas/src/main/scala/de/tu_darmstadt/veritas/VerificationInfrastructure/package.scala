package de.tu_darmstadt.veritas

import quiver.{LEdge, LNode}

/**
  * Created by sylvia on 19/01/17.
  * collect a couple of type aliases
  */
package object VerificationInfrastructure {

  type ProofNode[S, P] = LNode[String, ProofStep[S, P]]
  type StrategyEdge[S, P] = LEdge[String, VerificationStrategy]
  type VerificationEdge[S, P] = LEdge[String, VerificationEdgeLabel[S, P]]

  def makeProofNode[S, P](ps: ProofStep[S, P]): ProofNode[S, P] = LNode(ps.name, ps)


}
