package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * return status of a prover call
  * Proved and Disproved include information on which transformer and which  prover achieved this result
  */
sealed trait ProverStatus

case class Proved[S, P, V](t: Transformer[S, P, V], p: Prover[V]) extends ProverStatus

case class Disproved[S, P, V](t: Transformer[S, P, V], p: Prover[V]) extends ProverStatus

//TODO: maybe add some more detailed information here as well?
case object Inconclusive extends ProverStatus

/**
  * Interface for concrete provers
  */
abstract class Prover[V](problem: V) {

  val supportedStrategies: Seq[VerificationStrategy]

  def callProver(): ProverStatus
}
