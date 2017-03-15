package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.ProofGraph.{ProofEdges, ProofEdgesWithResult}

/**
  * Tactics for labeling edges of ProofTrees
  */
trait Tactic[Spec, Goal] extends Ordered[Tactic[Spec, Goal]] {
  def allRequiredGoalsVerified(step: ProofStep[Spec, Goal], edges: ProofEdgesWithResult[Spec, Goal]): Boolean =
    edges.forall { case (_, _, _, subgoalVerified) => subgoalVerified }

  /**
    * verifying a step via its edges generates a step result
    * the caller has to decide whether this result will be integrated into a proof graph or not
    * @param step
    * @param edges
    * @param verifier
    * @return
    */
  def verifyStep(step: ProofStep[Spec, Goal], edges: ProofEdges[Spec, Goal], verifier: Verifier[Spec, Goal]): StepResult[Spec, Goal] =
    verifier.verify(step.goal, step.spec, edges.map(_._1.goal))

  /**
    * applying a tactic to a ProofStep returns the edges generated from this application
    * edges include edge labels and sub-ProofSteps
    * caller has to decide whether the edges will be integrated into a proof graph or not
    * @param step
    * @return
    */
  def apply(step: ProofStep[Spec, Goal]): Option[ProofEdges[Spec, Goal]]
}

/**
  * default tactic: simply try to figure out a proof for a node of a proof graph
  * given its children, e.g. calling an ATP
  */

case class Solve[Spec, Goal]() extends Tactic[Spec, Goal] {
  override def compare(that: Tactic[Spec, Goal]): Int = that match {
    case that: Solve[Spec, Goal] => 0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
   }

  /* applying the Solve tactic cannot generate any edges */
  override def apply(step: ProofStep[Spec, Goal]): Option[ProofEdges[Spec, Goal]] = None

}


case class StructuralInduction[Spec <: Ordered[Spec], Goal <: Ordered[Goal]](inductionvar: Spec) extends Tactic[Spec, Goal] {
  //TODO we might have to refine the verifier call for induction once we really support this via a prover
  override def verifyStep(step: ProofStep[Spec, Goal], edges: ProofEdges[Spec, Goal], verifier: Verifier[Spec, Goal]): StepResult[Spec, Goal] = super.verifyStep(step, edges, verifier)


  override def compare(that: Tactic[Spec, Goal]): Int = that match {
    case that: StructuralInduction[Spec, Goal] => this.inductionvar compare that.inductionvar
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }

  override def apply(step: ProofStep[Spec, Goal]): Option[ProofEdges[Spec, Goal]] = ???
}

case class CaseDistinction[Spec, Goal]() extends Tactic[Spec, Goal] {

  override def compare(that: Tactic[Spec, Goal]): Int = that match {
    case that: CaseDistinction[Spec, Goal] => 0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }

  override def apply(step: ProofStep[Spec, Goal]): Option[ProofEdges[Spec, Goal]] = ???
}

//TODO which other abstract tactics are there for verifying proof graphs?

case class VerifierConfiguration[S, P, V](
                                           transformer: Transformer[S, P, V],
                                           strat: Tactic[S, P],
                                           prover: Prover[V],
                                           usedEdges: Seq[(ProofEdgeLabel, ProofStep[S, P])],
                                           verifier: Verifier[S, P])
