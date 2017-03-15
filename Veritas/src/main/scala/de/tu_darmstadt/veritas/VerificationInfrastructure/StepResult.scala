package de.tu_darmstadt.veritas.VerificationInfrastructure

import scala.util.Random

trait StepResult[S, P] {
  def status: VerifierStatus[S, P]
  def evidence: Option[Evidence]
  def errorMsg: Option[String]
}

trait Evidence

object Evidence {
  type EvidenceChecker[Ev <: Evidence] = Ev => Boolean
  type AnyEvidenceChecker = EvidenceChecker[Evidence]

  val trusting: EvidenceChecker[Evidence] = (_) => true

  def sampling[Ev <: Evidence](rate: Double, checker: EvidenceChecker[Ev]): EvidenceChecker[Ev] = (ev: Ev) =>
    if (Random.nextDouble() < rate)
      checker(ev)
    else
      true
}
