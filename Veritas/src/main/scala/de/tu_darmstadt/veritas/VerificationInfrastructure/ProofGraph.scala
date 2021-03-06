package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.Evidence.{AnyEvidenceChecker, EvidenceChecker}
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.Tactic
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{Verifier, VerifierStatus}

import scala.collection.mutable


trait EdgeLabel extends Serializable {
  def desc: String
  def propagateInfoList: Seq[PropagatableInfo]
}

trait PropagatableInfo {
  type P
  def propagateInfo(): P
}

trait GenProofStep[Spec, Goal] {
  def tactic: Tactic[Spec, Goal]
}

trait GenObligation[Spec, Goal] {
  def spec: Spec
  def goal: Goal
  def problemName: String
}
trait ObligationProducer[Spec, Goal, Obligation] {
  def newObligation(spec: Spec, goal: Goal): Obligation
  def newObligation(spec: Spec, goal: Goal, name: String): Obligation
//  def findOrCreateNewObligation(spec: Spec, goal: Goal): Obligation
//  def findOrStoreNewObligation(name: String, spec: Spec, goal: Goal): Obligation
}

trait GenStepResult[Spec, Goal] {
  def status: VerifierStatus[Spec, Goal]
  def evidence: Option[Evidence]
  def errorMsg: Option[String]
}
trait StepResultProducer[Spec, Goal, StepResult <: GenStepResult[Spec, Goal]] {
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
  def newObligation(spec: Spec, goal: Goal, name: String = "GeneratedObligation"): Obligation =
    obligationProducer.newObligation(spec, goal, name)

  val stepResultProducer: StepResultProducer[Spec, Goal, StepResult]
  def newStepResult(status: VerifierStatus[Spec, Goal], evidence: Option[Evidence], errorMsg: Option[String]): StepResult =
    stepResultProducer.newStepResult(status, evidence, errorMsg)

  def storedObligations: Map[String, Obligation]
  def findObligation(name: String): Option[Obligation]

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

  //TODO are two methods necessary here?
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
  //currently, standard traversals are in the ProofGraphTraversals trait

}

/** operations for modifying proof graphs:
 * - add or remove root obligations
 * - apply or unapply a tactic to an obligation, yielding a proof step and subobligations
 * - setting or unsetting the result of validating a proof step
 */
trait ProofGraph[Spec, Goal] extends IProofGraph[Spec, Goal] {

  /** Stores an obligation under the given name.
    * @return obligation previously stored under the same name, if any
    */
  def storeObligation(name: String, obl: Obligation): Option[Obligation]

  /** Removes obligation from the stored obligations. The obligation will remain in
    * the graph if it is currently required by other obligations, but it will not be
    * accessible via `findObligation` or `storedObligations`.
    */
  def unstoreObligation(obl: Obligation)

  def storeNewObligation(name: String, spec: Spec, goal: Goal): Obligation = {
    val obl = newObligation(spec, goal)
    storeObligation(name, obl)
    obl
  }

  // TODO add error possibility (applying a tactic could fail)
  def applyTactic(obl: Obligation, tactic: Tactic[Spec, Goal]): ProofStep
  def unapplyTactic(obl: Obligation)

  def setVerifiedBy(step: ProofStep, result: StepResult)
  def unsetVerifiedBy(step: ProofStep)

  def verifyProofStep(step: ProofStep, verifier: Verifier[Spec, Goal], pathforlogs: Option[String] = None): StepResult = {
    val oblToProve = targetedObl(step)
    val result = step.tactic.verifyStep(oblToProve, requiringSteps(oblToProve) map (_._2),
      requiredObls(step) map (p => (p._2, p._1)), verifier, stepResultProducer, pathforlogs)
    setVerifiedBy(step, result)
    result
  }


  /**
    * Proof graphs support dependency injection for registering evidence checkers.
    * If no checker is registered for a given evidence class, the proof graph defaults to @link{defaultEvidencenChecker}.
    */
  private val evidenceCheckers: mutable.Map[Class[_ <: Evidence], AnyEvidenceChecker] = mutable.Map()
  var defaultEvidencenChecker: AnyEvidenceChecker = Evidence.trusting //TODO rethink default!
  override def lookupEvidenceChecker(evClass: Class[_ <: Evidence]): AnyEvidenceChecker =
    evidenceCheckers.getOrElse(evClass, defaultEvidencenChecker)
  def registerEvidenceChecker[Ev <: Evidence](evClass: Class[Ev], checker: EvidenceChecker[Ev]) =
    evidenceCheckers += evClass -> checker.asInstanceOf[AnyEvidenceChecker]
}

object Versioned {
  val HEAD: VID = 0l
  type VID = Long
}
trait Versioned {
  import Versioned.VID
  type Obligation

  /**
    * - If obl exists in HEAD and obl exists in vid:
    *     Restores proof step of obl and performs recursive checkout of required subobligations.
    *     Note that obl is identical (same spec, same goal) in both HEAD and vid, hence all requiring steps remain valid.
    * - If obl exists in HEAD but does not exist in vid:
    *     Deletes obl.
    * - If obl does not exist in HEAD but does exist in vid:
    *     Nothing happens unless this occurs as part of a recursive checkout, in which case we introduce obl.
    * - If obl does not exist in HEAD and does not exist in vid:
    *     Nothing happens.
    */
  def checkoutObl(obl: Obligation, vid: VID)

  /**
    * If name exists in HEAD and name exists in vid but findObligation(name)@HEAD != findObligation(name)@vid:
    *     Throws exception since the checkout could potentially invalidate requiring steps.
    * In all other cases: use checkoutObl
    */
  def checkoutStoredObl(name: String, vid: VID)
}