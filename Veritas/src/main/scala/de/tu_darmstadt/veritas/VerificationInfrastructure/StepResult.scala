package de.tu_darmstadt.veritas.VerificationInfrastructure

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

  val trust: EvidenceChecker[VerificationEvidence] = _ => true
}
