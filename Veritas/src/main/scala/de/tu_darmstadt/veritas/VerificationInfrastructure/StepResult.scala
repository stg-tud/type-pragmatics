package de.tu_darmstadt.veritas.VerificationInfrastructure

import scala.util.Random

trait StepResult {
  def status: VerificationStatus
  def evidence: Option[VerificationEvidence]
  def errorMsg: Option[String]
  def completeLog: String

  def isStepVerified = status match {
    case Finished(reports) => reports.values.exists(_.isVerified)
    case _ => false
  }
}

trait VerificationEvidence
object VerificationEvidence {
  type EvidenceChecker[Ev <: VerificationEvidence] = Ev => Boolean

  val trusting: EvidenceChecker[VerificationEvidence] = _ => true

  def sampling[Ev <: VerificationEvidence](rate: Double, checker: EvidenceChecker[Ev]): EvidenceChecker[Ev] = (ev: Ev) =>
    if (Random.nextDouble() < rate)
      checker(ev)
    else
      true
}
