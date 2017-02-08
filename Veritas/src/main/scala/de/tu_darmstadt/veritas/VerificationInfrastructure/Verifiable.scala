package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * trait for marking parts of proof graphs (and maybe later other structures)
  * as "verifiable" (i.e. add a method that verifies the structure when given a Verifier)
  * - manages VerificationStatus!
  *
  */
trait Verifiable[S, P] {
  /**
    * verificationStatus is publicly readable,
    * but only methods within the verifiable instance may update the status!
    */
  protected val verificationStatus: VerificationStatus = NotStarted

  def getVerificationStatus: VerificationStatus = verificationStatus

  def verify(verifier: Verifier[S, P]): Verifiable[S, P]

  //TODO: this method probably requires a couple of parameters
  def makeOutdated(): Verifiable[S, P]

}

/**
  * triple for saving a particular configuration
  * @param transformer transformer used for a particular proof attempt
  * @param strategy verification strategy used for a particular proof attempt
  * @param prover prover used for a particular proof attempt
  * @tparam S type of the specification format
  * @tparam P type of the format for defining properties
  * @tparam V type of the verification format
  */
case class VerificationConfiguration[S, P, V](transformer: Transformer[S, P, V],
                                              strategy: VerificationStrategy,
                                              prover: Prover[V])

/**
  * status of a particular verification attempt (edges and leaves in the proof graph)
  * (including some more information about how this status was achieved)
  */
sealed trait VerificationStatus {
  val isVerified: Boolean = false
}

case object NotStarted extends VerificationStatus

//case class Outdated[S, P](prevs: VerificationStatus, previousProofGraph: ProofGraph[S, P]) extends VerificationStatus

case class Finished[S, P, V](report: Map[VerificationConfiguration[S, P, V], ProverStatus]) extends VerificationStatus {
  override val isVerified: Boolean = bestStatus().isVerified

  /***
    *
    * @return key-value-pair from report map that indicates "the best status" of the ones present
    */
  private def bestAttempt(): (VerificationConfiguration[S, P, V], ProverStatus) = ???

  def bestConf(): VerificationConfiguration[S, P, V] = bestAttempt()._1

  def bestStatus(): ProverStatus = bestAttempt()._2
}

/**
  * this status
  * - indicates any problem produced during transformation steps
  * - indicates contradicting prover results
  *
  * //TODO maybe refine errorMessage later (e.g. introduce particular case class
  * @param errorMessage message describing the problem
  */
case class VerificationFailure(errorMessage: String) extends VerificationStatus

