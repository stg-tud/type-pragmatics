package de.tu_darmstadt.veritas.VerificationInfrastructure

trait StepResult {
  def status: VerificationStatus
  def evidence: Option[VerificationEvidence]
  def errorMsg: Option[String]
  def completeLog: String

  def isStepVerified = status match {
    case Finished(reports) => reports.values.exists(_.isVerified)
  }
}

trait VerificationEvidence

