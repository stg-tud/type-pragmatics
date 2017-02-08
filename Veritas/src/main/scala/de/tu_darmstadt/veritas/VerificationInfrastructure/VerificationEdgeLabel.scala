package de.tu_darmstadt.veritas.VerificationInfrastructure


//TODO: problem - do we really need to explicitly pass the full information from the ends of the edge (spec, goal, assms)?
class VerificationEdgeLabel[S, P](val vstrat: VerificationStrategy,
                                  spec: S, goal: P, assms: Seq[P])
  extends Verifiable[S, P] {
  override def verify(verifier: Verifier[S, P]): VerificationEdgeLabel[S, P] =
    new VerificationEdgeLabel[S, P](vstrat, spec, goal, assms) {
      override val verificationStatus: VerificationStatus =
        verifier.verify(spec, assms, goal, vstrat)
    }

  //TODO: this method probably requires a couple of parameters
  override def makeOutdated(): VerificationEdgeLabel[S, P] = ???
}