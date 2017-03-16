package de.tu_darmstadt.veritas.VerificationInfrastructure

import scala.util.Random

trait Evidence extends Ordered[Evidence]

object Evidence {
  type EvidenceChecker[Ev <: Evidence] = Ev => Boolean
  type AnyEvidenceChecker = EvidenceChecker[Evidence]

  val failing: AnyEvidenceChecker = _ => false
  val trusting: AnyEvidenceChecker = _ => true

  def sampling[Ev <: Evidence](rate: Double, checker: EvidenceChecker[Ev]): EvidenceChecker[Ev] = (ev: Ev) =>
    if (Random.nextDouble() < rate)
      checker(ev)
    else
      true
}
