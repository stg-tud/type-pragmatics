package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * return status of a prover call
  */
sealed trait ProverStatus

case object Proved extends ProverStatus

case object Disproved extends ProverStatus

case class Inconclusive(terminationReason: String) extends ProverStatus

/**
  * Interface for concrete provers
  */
abstract class Prover[V](problem: V) {
  def callProver(): ProverStatus
}
