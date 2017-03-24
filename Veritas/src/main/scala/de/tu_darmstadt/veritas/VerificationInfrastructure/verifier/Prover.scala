package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

/**
  * return status of a prover call
  */
sealed trait ProverStatus extends Ordered[ProverStatus] {
  val isVerified: Boolean = false
  val proverLog: String

  override def compare(that: ProverStatus): Int = that match {
    case that: this.type => proverLog compare that.proverLog
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
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
trait Prover[V <: VerifierFormat] {

  def callProver(problem: V): ProverStatus
}