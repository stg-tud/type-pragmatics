package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * return status of a prover call
  * Proved and Disproved include information on which transformer and which prover achieved this result
  */
sealed trait ProverStatus {
  val isVerified: Boolean = false
  val proverLog: String //TODO have this string or not?
}

case class Proved(plog: String) extends ProverStatus {
  override val isVerified: Boolean = true
  override val proverLog: String = plog
}

case class Disproved(plog: String) extends ProverStatus {
  override val proverLog: String = plog
}


//TODO: maybe add some more detailed information here as well?
case class Inconclusive(plog: String) extends ProverStatus {
  override val proverLog: String = plog
}

case class ProverFailure(plog: String) extends ProverStatus {
  override val proverLog: String = plog
}

/**
  * Interface for concrete provers
  */
abstract class Prover[+V](problem: V) {

  val supportedStrategies: Seq[VerificationStrategy]

  def callProver(): ProverStatus
}
