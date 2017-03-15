package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.Evidence.{AnyEvidenceChecker, EvidenceChecker}
import de.tu_darmstadt.veritas.VerificationInfrastructure.ProofGraph.ProofEdges

import scala.collection.mutable

trait ProofGraph[Spec, Goal] {

  //TODO: some of methods in ProofGraph might have to check first whether their step argument actually exists in the graph!

  /* graph management */
  def rootSteps: Iterable[ProofStep[Spec, Goal]]

  def addProofStep(step: ProofStep[Spec, Goal])
  /** @throws IllegalArgumentException when this edge would form a cycle in the graph. */
  def addProofEdge(from: ProofStep[Spec, Goal], to: ProofStep[Spec, Goal], label: ProofEdgeLabel)

  def removeProofStep(step: ProofStep[Spec, Goal])
  def removeProofEdge(from: ProofStep[Spec, Goal], to: ProofStep[Spec, Goal], label: ProofEdgeLabel)

  def requires(step: ProofStep[Spec, Goal]): ProofEdges[Spec, Goal]
  def requiredBy(step: ProofStep[Spec, Goal]): ProofEdges[Spec, Goal]


  /* verification status management */
  def setVerifiedBy(step: ProofStep[Spec, Goal], result: StepResult[Spec, Goal])
  def getVerifiedBy(step: ProofStep[Spec, Goal]): Option[StepResult[Spec, Goal]]

  def isStepVerified(step: ProofStep[Spec, Goal]): Boolean = getVerifiedBy(step).map(_.isStepVerified).getOrElse(false)
  def isGoalVerified(step: ProofStep[Spec, Goal]): Boolean = computeIsGoalVerified(step)

  def computeIsGoalVerified(step: ProofStep[Spec, Goal]): Boolean =
    if (isStepVerified(step)) {
      val requiredSteps = requires(step).map { case (substep, label) => (substep, label, getVerifiedBy(substep), isGoalVerified(substep)) }
      step.tactic.allRequiredGoalsVerified(step, requiredSteps)
    }
    else
      false

  def updateStep(oldstep: ProofStep[Spec, Goal], newstep: ProofStep[Spec, Goal])

  /* check if a given step is indeed part of the graph) */
  def stepExists(step: ProofStep[Spec, Goal]): Boolean

  def applyTactic(step: ProofStep[Spec, Goal], tactic: Tactic[Spec, Goal]): Unit =
    if (stepExists(step)) {
      //TODO add some error handling (applying the tactic could fail)
      val newedges = tactic.apply(step)
      for (oe <- newedges; (ps, e) <- oe) {
        addProofStep(ps)
        addProofEdge(step, ps, e)
      }
      val newstep = ProofStep(step.spec, step.goal, tactic)
      updateStep(step, newstep)
    }

  /**
    * Proof graphs support dependency injection for registering evidence checkers.
    * If no checker is registered for a given evidence class, the proof graph defaults to @link{defaultEvidencenChecker}.
    */
  private val evidenceCheckers: mutable.Map[(Class[_ <: Evidence], Class[_ <: VerifierFormat]), AnyEvidenceChecker] = mutable.Map()

  var defaultEvidencenChecker: AnyEvidenceChecker
  def registerEvidenceChecker[Ev <: Evidence, V <: VerifierFormat](evClass: Class[Ev], vClass: Class[V], checker: EvidenceChecker[Ev, V]) =
    evidenceCheckers += (evClass, vClass) -> checker
  def lookupEvidenceChecker[Ev <: Evidence, V <: VerifierFormat](evClass: Class[Ev], vClass: Class[V]): AnyEvidenceChecker =
    evidenceCheckers.getOrElse((evClass, vClass), defaultEvidencenChecker)


  /* traversals */

  // TODO what traversals do we need?

}


object ProofGraph {
  type ProofEdges[Spec, Goal] = Iterable[(ProofStep[Spec, Goal], ProofEdgeLabel)]
  type ProofEdgesWithResult[Spec, Goal] = Iterable[(ProofStep[Spec, Goal], ProofEdgeLabel, Option[StepResult[Spec, Goal]], Boolean)]
}

