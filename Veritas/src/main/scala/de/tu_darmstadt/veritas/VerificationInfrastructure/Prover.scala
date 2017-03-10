package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * return status of a prover call
  */
sealed trait ProverStatus {
  val isVerified: Boolean = false
  val proverLog: String
}

case class Proved(proverLog: String) extends ProverStatus {
  override val isVerified: Boolean = true
}

case class Disproved(proverLog: String) extends ProverStatus

case class Inconclusive(proverLog: String) extends ProverStatus

case class ProverFailure(proverLog: String) extends ProverStatus

/**
  * Interface for concrete provers
  */
trait Prover[V] {

  def supportedStrategies[S, P](): Seq[VerificationStrategy[S, P]]

  def callProver(problem: V): ProverStatus
}
