package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * return status of a prover call
  * Proved and Disproved include information on which transformer and which prover achieved this result
  */
sealed trait ProverStatus {
  val isVerified: Boolean = false
}

case class Proved[+V](p: Prover[V]) extends ProverStatus {
  override val isVerified: Boolean = true
}

case class Disproved[+V](p: Prover[V]) extends ProverStatus

//TODO: maybe add some more detailed information here as well?
case object Inconclusive extends ProverStatus

/**
  * Interface for concrete provers
  */
// TODO need a transformer otherwise we can not create a ProverStatus object
// -> why? can we somehow separate this? Prover should not have to care about transforming a problem...
abstract class Prover[+V](problem: V) {

  val supportedStrategies: Seq[VerificationStrategy]

  def callProver(): ProverStatus
}
