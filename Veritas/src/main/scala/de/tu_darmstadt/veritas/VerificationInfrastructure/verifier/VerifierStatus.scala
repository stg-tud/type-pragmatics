package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

/**
  * status of a particular verification attempt (for a node/leaf in a proof tree)
  *
  */
sealed trait VerifierStatus[Spec, Goal] extends Ordered[VerifierStatus[Spec, Goal]] with Serializable {
  val verifier: Verifier[Spec, Goal]
  val isVerified: Boolean = false
}

case class Unknown[Spec, Goal](verifier: Verifier[Spec, Goal]) extends VerifierStatus[Spec, Goal] {
  override def compare(that: VerifierStatus[Spec, Goal]): Int = that match {
    case that: Unknown[_, _] => this.verifier.desc compare that.verifier.desc
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

//TODO: maybe refine errorMessage: String later to include specific error objects
case class VerifierFailure[Spec, Goal](errorMessage: String, verifier: Verifier[Spec, Goal]) extends VerifierStatus[Spec, Goal] {
  import Ordered._
  override def compare(that: VerifierStatus[Spec, Goal]): Int = that match {
    case that: VerifierFailure[_, _] => (this.errorMessage, this.verifier.desc) compare (that.errorMessage, that.verifier.desc)
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}


case class Finished[Spec, Goal](status: ProverStatus, verifier: Verifier[Spec, Goal]) extends VerifierStatus[Spec, Goal] {
  override val isVerified = status.isVerified

  override def compare(that: VerifierStatus[Spec, Goal]): Int = that match {
    case that: Finished[_, _] => (this.status, this.verifier.desc) compare (that.status, that.verifier.desc)
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

//TODO support status that contains several different Prover stati?
//TODO what if a Verifier does not generate a ProverStatus?

