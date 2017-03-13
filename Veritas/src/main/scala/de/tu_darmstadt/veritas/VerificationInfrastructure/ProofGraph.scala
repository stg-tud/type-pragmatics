package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.VerificationEvidence.EvidenceChecker

import scala.collection.mutable

trait ProofGraph[Spec, Goal] {

  /* graph management */
  def rootSteps: Iterable[ProofStep[Spec, Goal]]

  def addProofStep(step: ProofStep[Spec, Goal])
  def addProofEdge(from: ProofStep[Spec, Goal], to: ProofStep[Spec, Goal], label: ProofEdgeLabel)

  def removeProofStep(step: ProofStep[Spec, Goal])
  def removeProofEdge(from: ProofStep[Spec, Goal], to: ProofStep[Spec, Goal], label: ProofEdgeLabel)

  def requires(step: ProofStep[Spec, Goal]): Iterable[(ProofStep[Spec, Goal], ProofEdgeLabel)]
  def requiredBy(step: ProofStep[Spec, Goal]): Iterable[(ProofStep[Spec, Goal], ProofEdgeLabel)]


  /* verification status management */
  def setVerifiedBy(step: ProofStep[Spec, Goal], result: StepResult)
  def getVerifiedBy(step: ProofStep[Spec, Goal]): Option[StepResult]

  def isStepVerified(step: ProofStep[Spec, Goal]): Boolean
  def isGoalVerified(step: ProofStep[Spec, Goal]): Boolean

  /**
    * Proof graphs support dependency injection for registering evidence checkers.
    * If no checker is registered for a given evidence class, the proof graph defaults to @link{defaultEvidencenChecker}.
    */
  val evidenceCheckers: mutable.Map[Class[_ <: VerificationEvidence], EvidenceChecker[_ <: VerificationEvidence]] = mutable.Map()

  var defaultEvidencenChecker: EvidenceChecker[VerificationEvidence]
  def registerEvidenceChecker[Ev <: VerificationEvidence](evClass: Class[Ev], checker: EvidenceChecker[Ev]) =
    evidenceCheckers += evClass -> checker
  def lookupEvidenceChecker[Ev <: VerificationEvidence](evClass: Class[Ev]): EvidenceChecker[_ <: VerificationEvidence] =
    evidenceCheckers.getOrElse(evClass, defaultEvidencenChecker)


  /* traversals */

  // TODO what traversals do we need?

}

