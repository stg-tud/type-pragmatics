package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.Evidence.{AnyEvidenceChecker, EvidenceChecker}
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.Tactic

import scala.collection.mutable


trait EdgeLabel extends Ordered[EdgeLabel]

case class Obligation[Spec, Goal](spec: Spec, goal: Goal)

case class ProofStep[Spec, Goal](tactic: Tactic[Spec, Goal])


trait ProofGraph[Spec, Goal] {

  //TODO: some of methods in ProofGraph might have to check first whether their step argument actually exists in the graph!

  /* operations for modifying proof graphs:
   * - add or remove root obligations
   * - apply or unapply a tactic to an obligation, yielding a proof step and subobligations
   * - setting or unsetting the result of validating a proof step
   */

  def addRootObligation(obl: Obligation[Spec, Goal])
  def removeRootObligation(step: Obligation[Spec, Goal])

  def applyTactic(obl: Obligation[Spec, Goal], tactic: Tactic[Spec, Goal]): ProofStep[Spec, Goal]
  def unapplyTactic(obl: Obligation[Spec, Goal])

  def setVerifiedBy(step: ProofStep[Spec, Goal], result: StepResult[Spec, Goal])
  def unsetVerifiedBy(step: ProofStep[Spec, Goal])


  /* operations for querying proof graphs:
   * - sequence of root proof obligations
   * - navigating from obligations to used proof step to required subobligations
   * - navigating from subobligations to requiring proof steps to targeted obligation
   * - retrieving step result if any and checking whether a step was successfully verified
   * - checking whether an obligation was successfully verified
   */

  def rootObligations: Iterable[Obligation[Spec, Goal]]

  /** Yields proof step if any */
  def appliedStep(obl: Obligation[Spec, Goal]): Option[ProofStep[Spec, Goal]]
  /** Yields required subobligations */
  def requiredObls(step: ProofStep[Spec, Goal]): Iterable[(Obligation[Spec, Goal], EdgeLabel)]

  /** Yields proof steps that require the given obligation */
  def requiringSteps(obligation: Obligation[Spec, Goal]): Iterable[(ProofStep[Spec, Goal], EdgeLabel)]
  /** Yields the obligation the proof step was applied to */
  def targetedObl(step: ProofStep[Spec, Goal]): Obligation[Spec, Goal]

  def getVerifiedBy(step: ProofStep[Spec, Goal]): Option[StepResult[Spec, Goal]]
  def isStepVerified(step: ProofStep[Spec, Goal]): Boolean = getVerifiedBy(step) match {
    case None => false
    case Some(result) if !result.status.isVerified => false
    case Some(result) if result.evidence.isDefined => lookupEvidenceChecker(result.evidence.get.getClass)(result.evidence.get)
    case Some(result) => defaultEvidencenChecker(null)
  }

  def isOblVerified(obl: Obligation[Spec, Goal]): Boolean = computeIsGoalVerified(obl)
  def computeIsGoalVerified(obl: Obligation[Spec, Goal]): Boolean = appliedStep(obl) match {
    case None =>  false
    case Some(step) =>
      if (isStepVerified(step)) {
        val required = requiredObls(step).map { case (subobl, label) =>
          val verifiedBy = appliedStep(subobl).flatMap(getVerifiedBy(_))
          val suboblVerified = isOblVerified(subobl)
          (subobl, label, verifiedBy, suboblVerified)
        }
        step.tactic.allRequiredOblsVerified(obl, required)
      }
      else
        false
  }

  /**
    * Proof graphs support dependency injection for registering evidence checkers.
    * If no checker is registered for a given evidence class, the proof graph defaults to @link{defaultEvidencenChecker}.
    */
  private val evidenceCheckers: mutable.Map[Class[_ <: Evidence], AnyEvidenceChecker] = mutable.Map()

  var defaultEvidencenChecker: AnyEvidenceChecker
  def registerEvidenceChecker[Ev <: Evidence](evClass: Class[Ev], checker: EvidenceChecker[Ev]) =
    evidenceCheckers += evClass -> checker.asInstanceOf[AnyEvidenceChecker]
  def lookupEvidenceChecker(evClass: Class[_ <: Evidence]): AnyEvidenceChecker =
    evidenceCheckers.getOrElse(evClass, defaultEvidencenChecker)


  /* traversals */

  // TODO what traversals do we need/want to offer?

}


object ProofGraph {
  type ProofEdges[Spec, Goal] = Iterable[(Obligation[Spec, Goal], EdgeLabel)]
  type ProofEdgesWithResult[Spec, Goal] = Iterable[(Obligation[Spec, Goal], EdgeLabel, Option[StepResult[Spec, Goal]], Boolean)]
}

