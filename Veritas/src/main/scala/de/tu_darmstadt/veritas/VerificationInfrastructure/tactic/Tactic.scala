package de.tu_darmstadt.veritas.VerificationInfrastructure.tactic

import de.tu_darmstadt.veritas.VerificationInfrastructure.ProofGraph.{ProofEdges, ProofEdgesWithResult}
import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, Obligation, StepResult, Verifier}

/**
  * Tactics for labeling edges of ProofTrees
  */
trait Tactic[Spec, Goal] extends Ordered[Tactic[Spec, Goal]] {
  def allRequiredOblsVerified(step: Obligation[Spec, Goal], edges: ProofEdgesWithResult[Spec, Goal]): Boolean =
    edges.forall { case (_, _, _, subgoalVerified) => subgoalVerified }

  /**
    * verifying a step via its edges generates a step result
    * the caller has to decide whether this result will be integrated into a proof graph or not
    * @param step
    * @param edges
    * @param verifier
    * @return
    */
  def verifyStep(step: Obligation[Spec, Goal], edges: ProofEdges[Spec, Goal], verifier: Verifier[Spec, Goal]): StepResult[Spec, Goal] =
    verifier.verify(step.goal, step.spec, edges.map(_._1.goal))

  /**
    * applying a tactic to a ProofStep returns the edges generated from this application
    * edges include edge labels and sub-ProofSteps
    * caller has to decide whether the edges will be integrated into a proof graph or not
    * @param obl
    * @throws TacticApplicationException
    * @return
    */
  def apply(obl: Obligation[Spec, Goal]): Iterable[(Obligation[Spec, Goal], EdgeLabel)]
}

trait TacticApplicationException[Spec, Goal] extends Exception {
  val tactic: Tactic[Spec, Goal]
}