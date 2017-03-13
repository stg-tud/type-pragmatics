package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.Evidence.{AnyEvidenceChecker, EvidenceChecker}
import de.tu_darmstadt.veritas.VerificationInfrastructure.ProofGraph.ProofEdges

import scala.collection.mutable

trait ProofGraph[Spec, Goal] {

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

  def isStepVerified(step: ProofStep[Spec, Goal]): Boolean
  def isGoalVerified(step: ProofStep[Spec, Goal]): Boolean

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
}

