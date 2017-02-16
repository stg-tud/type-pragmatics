package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * Strategies for labeling edges of ProofTrees
  */
abstract class VerificationStrategy[S, P] {
  def fullyVerified(edgeseq: Seq[(EdgeLabel, Boolean)]): Boolean
  def callVerifier(verifier: Verifier[S, P], spec: S, goal: P, edges: Seq[(EdgeLabel, ProofStep[S ,P])]): VerificationStatus
}

/**
  * default strategy: simply try to figure out a proof for a node of a proof tree given its children, e.g. calling an ATP
  */

case class Solve[S, P]() extends VerificationStrategy[S, P] {
  override def fullyVerified(edgeseq: Seq[(EdgeLabel, Boolean)]): Boolean = {
    edgeseq.forall { e => e._2 }
  }

  override def callVerifier(verifier: Verifier[S, P], spec: S, goal: P, edges: Seq[(EdgeLabel, ProofStep[S, P])]): VerificationStatus = {
    val hypotheses = edges.map { e => (e._1, e._2.goal)}
    verifier.verify(spec, hypotheses, goal, this)
  }
}

// TODO: is only a copy of Solve to have different subclasses
case class Induction[S, P]() extends VerificationStrategy[S, P] {
  override def fullyVerified(edgeseq: Seq[(EdgeLabel, Boolean)]): Boolean = {
    edgeseq.forall { e => e._2 }
  }

  override def callVerifier(verifier: Verifier[S, P], spec: S, goal: P, edges: Seq[(EdgeLabel, ProofStep[S, P])]): VerificationStatus = {
    val hypotheses = edges.map { e => (e._1, e._2.goal)}
    verifier.verify(spec, hypotheses, goal, this)
  }
}

//TODO maybe refine this later
//case object Induction extends VerificationStrategy

//TODO which other abstract strategies are there for verifying proof trees?

case class VerificationConfiguration[S, P, V](
  val transformer: Transformer[S, P, V],
  val strat: VerificationStrategy[S, P],
  val prover: Prover[V],
  val usedEdges: Seq[(EdgeLabel, ProofStep[S, P])],
  val verifier: Verifier[S, P])
