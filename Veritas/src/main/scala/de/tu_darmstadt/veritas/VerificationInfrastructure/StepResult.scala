package de.tu_darmstadt.veritas.VerificationInfrastructure

import scala.util.Random

trait StepResult[S, P] {
  def status: VerifierStatus[S, P]
  def evidence: Option[Evidence]
  def errorMsg: Option[String]

  def isStepVerified: Boolean = status.isVerified
  def isGoalVerified: Boolean = ???
}

trait Evidence

object Evidence {
  type EvidenceChecker[Ev <: Evidence, V <: VerifierFormat] = (Ev, V) => Boolean
  type AnyEvidenceChecker = EvidenceChecker[_ <: Evidence, _ <: VerifierFormat]

  val trusting: EvidenceChecker[Evidence, VerifierFormat] = (_,_) => true

  def sampling[Ev <: Evidence, V <: VerifierFormat](rate: Double, checker: EvidenceChecker[Ev, V]): EvidenceChecker[Ev, V] = (ev: Ev, v: V) =>
    if (Random.nextDouble() < rate)
      checker(ev, v)
    else
      true
}
