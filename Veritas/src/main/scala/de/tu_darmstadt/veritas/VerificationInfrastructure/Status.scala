package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * status of a particular verification attempt (for a node/leaf in a proof tree)
  *
  */
sealed trait VerifierStatus extends Ordered[VerifierStatus] {
  val isVerified: Boolean = false
}

case object Unknown extends VerifierStatus {
  override def compare(that: VerifierStatus): Int = that match {
    case that: Unknown.type => 0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

//TODO: maybe refine errorMessage: String later to include specific error objects
class Failure[S, P](val errorMessage: String,
                    val usedVerifier: Verifier[S, P],
                    val prevs: Option[VerifierStatus],
                    val previousProofGraph: Option[ProofGraphQuiver[S, P]]) extends VerifierStatus {
  override def compare(that: VerifierStatus): Int = that match {
    case that: Failure[S, P] => errorMessage compare that.errorMessage
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

//allow different constructors for VerificationFailure
object Failure {
  def apply[S, P](em: String, usedVerifier: Verifier[S, P]) =
    new Failure[S, P](em, usedVerifier, None, None)

  def apply[S, P](errorMessage: String, usedVerifier: Verifier[S, P],
                  prevs: Option[VerifierStatus], previousProofGraph: Option[ProofGraphQuiver[S, P]]) =
    new Failure[S, P](errorMessage, usedVerifier, prevs, previousProofGraph)

  def unapply[S, P](arg: Failure[S, P]): Option[(String, Verifier[S, P], Option[VerifierStatus], Option[ProofGraphQuiver[S, P]])] =
    Some((arg.errorMessage, arg.usedVerifier, arg.prevs, arg.previousProofGraph))

}

case class Finished[S, P, V](report: Map[VerificationConfiguration[S, P, V], ProverStatus]) extends VerifierStatus {
  private lazy val bestAttempt: (VerificationConfiguration[S, P, V], ProverStatus) = {
    def sortByProverStatus(ps: ProverStatus): Int = ps match {
      // proved <- disproved <- inconclusive <- ProverFailure
      case Proved(_) => 0
      case Disproved(_) => 1
      case Inconclusive(_) => 3
      case ProverFailure(_) => 4
    }

    report.toSeq.sortBy { case (_, ps) => sortByProverStatus(ps) }.head
  }

  lazy val bestConf: VerificationConfiguration[S, P, V] = bestAttempt._1

  lazy val bestStatus: ProverStatus = bestAttempt._2

  override val isVerified = bestStatus.isVerified

  override def compare(that: VerifierStatus): Int = that match {
    case that: Finished[S, P, V] => this.report.head._2 compare that.report.head._2
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

