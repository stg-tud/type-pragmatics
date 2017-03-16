package de.tu_darmstadt.veritas.VerificationInfrastructure.tactic

import de.tu_darmstadt.veritas.VerificationInfrastructure._

/**
  * Tactics for labeling edges of ProofTrees
  */
trait Tactic[Spec, Goal] extends Ordered[Tactic[Spec, Goal]] {
  def allRequiredOblsVerified(g: IProofGraph[Spec, Goal])(obl: g.Obligation, edges: Iterable[(g.Obligation, EdgeLabel)]): Boolean =
    edges.forall { case (subobl, label) => g.isOblVerified(subobl) }

  /**
    * verifying a step via its edges generates a step result
    * the caller has to decide whether this result will be integrated into a proof graph or not
    * @param obl
    * @param edges
    * @param verifier
    * @return
    */
  def verifyStep(g: IProofGraph[Spec, Goal])(obl: g.Obligation, edges: Iterable[(g.Obligation, EdgeLabel)], verifier: Verifier[Spec, Goal]): g.StepResult =
    verifier.verify(g)(obl.goal, obl.spec, edges.map(_._1.goal))

  /**
    * applying a tactic to a ProofStep returns the edges generated from this application
    * edges include edge labels and sub-ProofSteps
    * caller has to decide whether the edges will be integrated into a proof graph or not
    * @param obl
    * @throws TacticApplicationException
    * @return
    */
  def apply(g: IProofGraph[Spec, Goal])(obl: g.Obligation): Iterable[(g.Obligation, EdgeLabel)]
}

trait TacticApplicationException[Spec, Goal] extends Exception {
  val tactic: Tactic[Spec, Goal]
}