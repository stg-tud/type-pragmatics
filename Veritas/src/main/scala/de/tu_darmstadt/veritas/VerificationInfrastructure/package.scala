package de.tu_darmstadt.veritas

import quiver.{LEdge, LNode}

/**
  * Created by sylvia on 19/01/17.
  * collect a couple of type aliases
  */
package object VerificationInfrastructure {

  type ProofNode[S, P] = LNode[String, ProofStep[S, P]]
  type VerificationEdge = LEdge[String, VerificationStrategy]

}
