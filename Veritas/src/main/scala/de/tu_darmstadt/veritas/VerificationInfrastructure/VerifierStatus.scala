package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * status of a particular verification attempt (for a node/leaf in a proof tree)
  *
  */
sealed trait VerifierStatus[S, P] extends Ordered[VerifierStatus[S, P]] {
  val verifier: Verifier[S, P]
  val isVerified: Boolean = false
}

case class Unknown[S, P](verifier: Verifier[S, P]) extends VerifierStatus[S, P] {
  override def compare(that: VerifierStatus[S, P]): Int = that match {
    case that: Unknown[_, _] => this.verifier.desc compare that.verifier.desc
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

//TODO: maybe refine errorMessage: String later to include specific error objects
case class Failure[S, P](errorMessage: String, verifier: Verifier[S, P]) extends VerifierStatus[S, P] {
  import Ordered._
  override def compare(that: VerifierStatus[S, P]): Int = that match {
    case that: Failure[_, _] => (this.errorMessage, this.verifier.desc) compare (that.errorMessage, that.verifier.desc)
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}


case class Finished[S, P](status: ProverStatus, verifier: Verifier[S, P]) extends VerifierStatus[S, P] {
  override val isVerified = status.isVerified

  override def compare(that: VerifierStatus[S, P]): Int = that match {
    case that: Finished[_, _] => (this.status, this.verifier.desc) compare (that.status, that.verifier.desc)
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

