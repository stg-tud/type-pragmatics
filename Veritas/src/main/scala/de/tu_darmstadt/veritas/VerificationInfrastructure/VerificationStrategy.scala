package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.ProofGraph.{ProofEdges, ProofEdgesWithResult}

/**
  * Strategies for labeling edges of ProofTrees
  */
trait VerificationStrategy[S, P] extends Ordered[VerificationStrategy[S, P]] {
  def allRequiredGoalsVerified(step: ProofStep[S, P], edges: ProofEdgesWithResult[S, P]): Boolean =
    edges.forall { case (_, _, _, subgoalVerified) => subgoalVerified }

  def verifyStep(step: ProofStep[S, P], edges: ProofEdges[S, P], verifier: Verifier[S, P]): StepResult[S, P] =
    verifier.verify(step.goal, step.spec, edges.map(_._1.goal))
}

/**
  * default strategy: simply try to figure out a proof for a node of a proof tree given its children, e.g. calling an ATP
  */

case class Solve[S, P]() extends VerificationStrategy[S, P] {
  override def compare(that: VerificationStrategy[S, P]): Int = that match {
    case that: Solve[S, P] => 0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
   }
}

// below is only a copy of Solve from before induction was refined
//case class Induction[S, P]() extends VerificationStrategy[S, P] {
//  override def fullyVerified(edgeseq: Seq[(ProofEdgeLabel, Boolean)]): Boolean = {
//    edgeseq.forall { e => e._2 }
//  }
//
//  override def callVerifier(verifier: Verifier[S, P], spec: S, goal: P, edges: Seq[(ProofEdgeLabel, ProofStep[S, P])]): VerificationStatus = {
//    val hypotheses = edges.map { e => (e._1, e._2.goal)}
//    verifier.verify(spec, hypotheses, goal, this)
//  }
//}

case class StructuralInduction[S <: Ordered[S], P <: Ordered[P]](inductionvar: S) extends VerificationStrategy[S, P] {
  //TODO we might have to refine the verifier call for induction once we really support this via a prover
  override def verifyStep(step: ProofStep[S, P], edges: ProofEdges[S, P], verifier: Verifier[S, P]): StepResult[S, P] = super.verifyStep(step, edges, verifier)


  override def compare(that: VerificationStrategy[S, P]): Int = that match {
    case that: StructuralInduction[S, P] => this.inductionvar compare that.inductionvar
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

case class CaseDistinction[S, P]() extends VerificationStrategy[S, P] {

  override def compare(that: VerificationStrategy[S, P]): Int = that match {
    case that: CaseDistinction[S, P] => 0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

//TODO which other abstract strategies are there for verifying proof trees?

case class VerifierConfiguration[S, P, V](
                                               transformer: Transformer[S, P, V],
                                               strat: VerificationStrategy[S, P],
                                               prover: Prover[V],
                                               usedEdges: Seq[(ProofEdgeLabel, ProofStep[S, P])],
                                               verifier: Verifier[S, P])
