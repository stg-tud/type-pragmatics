package de.tu_darmstadt.veritas.VerificationInfrastructure

trait ProofGraph[Spec, Goal] {
  def addProofStep(step: ProofStep[Spec, Goal])
  def addProofEdge(from: ProofStep[Spec, Goal], to: ProofStep[Spec, Goal], label: ProofEdgeLabel)

  def removeProofStep(step: ProofStep[Spec, Goal])
  def removeProofEdge(from: ProofStep[Spec, Goal], to: ProofStep[Spec, Goal], label: ProofEdgeLabel)

  def setVerifiedBy(step: ProofStep[Spec, Goal], result: StepResult)
  def verifiedBy(step: ProofStep[Spec, Goal]): Option[StepResult]

  def isStepVerified(step: ProofStep[Spec, Goal]): Boolean
  def isGoalVerified(step: ProofStep[Spec, Goal]): Boolean

  def requires(step: ProofStep[Spec, Goal]): Iterable[(ProofStep[Spec, Goal], ProofEdgeLabel)]
  def requiredBy(step: ProofStep[Spec, Goal]): Iterable[(ProofStep[Spec, Goal], ProofEdgeLabel)]


}

