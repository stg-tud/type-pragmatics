package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.Evidence.{AnyEvidenceChecker, EvidenceChecker}
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.Tactic

import scala.collection.mutable


trait EdgeLabel extends Ordered[EdgeLabel]

trait GenProofStep[Spec, Goal] {
  def tactic: Tactic[Spec, Goal]
}

trait GenObligation[Spec, Goal] {
  def spec: Spec
  def goal: Goal
}
trait ObligationProducer[Spec, Goal, Obligation] {
  def newObligation(spec: Spec, goal: Goal): Obligation
}

trait GenStepResult[S, P] {
  def status: VerifierStatus[S, P]
  def evidence: Option[Evidence]
  def errorMsg: Option[String]
}
trait StepResultProducer[Spec, Goal, StepResult] {
  def newStepResult(status: VerifierStatus[Spec, Goal], evidence: Option[Evidence], errorMsg: Option[String]): StepResult
}
//case class GenStepResultImpl[S, P](status: VerifierStatus[S, P], evidence: Option[Evidence], errorMsg: Option[String]) extends GenStepResult[S, P]


/** operations for querying proof graphs (no changes possible):
  * - sequence of root proof obligations
  * - navigating from obligations to used proof step to required subobligations
  * - navigating from subobligations to requiring proof steps to targeted obligation
  * - retrieving step result if any and checking whether a step was successfully verified
  * - checking whether an obligation was successfully verified
  */
trait IProofGraph[Spec, Goal] {
  type ProofStep <: GenProofStep[Spec, Goal]
  type Obligation <: GenObligation[Spec, Goal]
  type StepResult <: GenStepResult[Spec, Goal]

  val obligationProducer: ObligationProducer[Spec, Goal, Obligation]
  def newObligation(spec: Spec, goal: Goal): Obligation =
    obligationProducer.newObligation(spec, goal)

  val stepResultProducer: StepResultProducer[Spec, Goal, StepResult]
  def newStepResult(status: VerifierStatus[Spec, Goal], evidence: Option[Evidence], errorMsg: Option[String]): StepResult =
    stepResultProducer.newStepResult(status, evidence, errorMsg)

  def rootObligations: Iterable[Obligation]

  /** Yields proof step if any */
  def appliedStep(obl: Obligation): Option[ProofStep]
  /** Yields required subobligations */
  def requiredObls(step: ProofStep): Iterable[(Obligation, EdgeLabel)]

  /** Yields proof steps that require the given obligation */
  def requiringSteps(obl: Obligation): Iterable[(ProofStep, EdgeLabel)]
  /** Yields the obligation the proof step was applied to */
  def targetedObl(step: ProofStep): Obligation

  def verifiedBy(step: ProofStep): Option[StepResult]
  def isStepVerified(step: ProofStep): Boolean = verifiedBy(step) match {
    case None => false
    case Some(result) if !result.status.isVerified => false
    case Some(result) if result.evidence.isDefined => lookupEvidenceChecker(result.evidence.get.getClass)(result.evidence.get)
    case Some(result) => defaultEvidencenChecker(null)
  }

  def isOblVerified(obl: Obligation): Boolean = computeIsGoalVerified(obl)
  def computeIsGoalVerified(obl: Obligation): Boolean = appliedStep(obl) match {
    case None =>  false
    case Some(step) =>
      if (isStepVerified(step))
        step.tactic.allRequiredOblsVerified(this)(obl, requiredObls(step))
      else
        false
  }

  /**
    * Proof graphs support dependency injection for registering evidence checkers.
    * If no checker is registered for a given evidence class, the proof graph defaults to @link{defaultEvidencenChecker}.
    */
  def defaultEvidencenChecker: AnyEvidenceChecker
  def lookupEvidenceChecker(evClass: Class[_ <: Evidence]): AnyEvidenceChecker


  /* traversals */

  // TODO what traversals do we need/want to offer?

}

trait ProofGraph[Spec, Goal] extends IProofGraph[Spec, Goal] {

  //TODO: some of methods in ProofGraph might have to check first whether their step argument actually exists in the graph!

  /** operations for modifying proof graphs:
   * - add or remove root obligations
   * - apply or unapply a tactic to an obligation, yielding a proof step and subobligations
   * - setting or unsetting the result of validating a proof step
   */

  def addRootObligation(obl: Obligation)
  def removeRootObligation(step: Obligation)

  def applyTactic(obl: Obligation, tactic: Tactic[Spec, Goal]): ProofStep
  def unapplyTactic(obl: Obligation)

  def setVerifiedBy(step: ProofStep, result: StepResult)
  def unsetVerifiedBy(step: ProofStep)

  def verifyProofStep(step: ProofStep, verifier: Verifier[Spec, Goal]): StepResult = {
    val result = step.tactic.verifyStep(targetedObl(step), requiredObls(step), verifier, stepResultProducer)
    setVerifiedBy(step, result)
    result
  }


  /**
    * Proof graphs support dependency injection for registering evidence checkers.
    * If no checker is registered for a given evidence class, the proof graph defaults to @link{defaultEvidencenChecker}.
    */
  private val evidenceCheckers: mutable.Map[Class[_ <: Evidence], AnyEvidenceChecker] = mutable.Map()
  var defaultEvidencenChecker: AnyEvidenceChecker = Evidence.failing
  override def lookupEvidenceChecker(evClass: Class[_ <: Evidence]): AnyEvidenceChecker =
    evidenceCheckers.getOrElse(evClass, defaultEvidencenChecker)
  def registerEvidenceChecker[Ev <: Evidence](evClass: Class[Ev], checker: EvidenceChecker[Ev]) =
    evidenceCheckers += evClass -> checker.asInstanceOf[AnyEvidenceChecker]
}
