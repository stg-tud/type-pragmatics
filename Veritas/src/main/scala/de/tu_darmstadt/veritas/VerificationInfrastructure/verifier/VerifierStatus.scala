package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

/**
  * status of a particular verification attempt (for a node/leaf in a proof tree)
  *
  */
sealed trait VerifierStatus[Spec, Goal] extends Serializable {
  val verifier: Verifier[Spec, Goal]
  val isVerified: Boolean = false
}

case class Unknown[Spec, Goal](verifier: Verifier[Spec, Goal]) extends VerifierStatus[Spec, Goal]

//TODO: maybe refine errorMessage: String later to include specific error objects
case class VerifierFailure[Spec, Goal](errorMessage: String, verifier: Verifier[Spec, Goal]) extends VerifierStatus[Spec, Goal]


case class Finished[Spec, Goal](status: ProverStatus, verifier: Verifier[Spec, Goal]) extends VerifierStatus[Spec, Goal] {
  override val isVerified = status.isVerified
}

//TODO support status that contains several different Prover stati?
//TODO what if a Verifier does not generate a ProverStatus?

