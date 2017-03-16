package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.Tactic

import scala.collection.GenSeq


trait VerifierFormat

/**
  * Verifiers "manage" verification attempts (i.e. compiling the problem, calling one or more provers,
  * starting/stopping a proof attempt...)
  *
  */
trait Verifier[S, P] extends Ordered[Verifier[S, P]] {
  type V <: VerifierFormat

  /** Textual description that should be unique (used for ordering verifiers) */
  val desc: String

  override def compare(that: Verifier[S, P]): Int = this.desc compare that.desc

  //  val transformer : GenSeq[Transformer[S, P, V]] //translate a given specification (S) + goal (P) to a format for verification (V)
//  val provers : GenSeq[Prover[V]] //sequence of provers that understand the given verification format (may be called in parallel)

  // TODO needed?
  val supportedStrategies: Seq[Tactic[S, P]] //general verification strategies that the provers can be called with

  def verify(goal: P, spec: S, assumptions: Iterable[P]): GenStepResult[S, P]

}
