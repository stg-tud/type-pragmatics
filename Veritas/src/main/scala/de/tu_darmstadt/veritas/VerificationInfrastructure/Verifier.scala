package de.tu_darmstadt.veritas.VerificationInfrastructure

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
  val transformer : GenSeq[Transformer[S, P, V]] //translate a given specification (S) + goal (P) to a format for verification (V)
  val provers : GenSeq[Prover[V]] //sequence of provers that understand the given verification format (may be called in parallel)
  val supportedStrategies: Seq[VerificationStrategy[S, P]] //general verification strategies that the provers can be called with
  // TODO: compute intersection of strategies that are supported

  /**
    * combine calling all transformers with all provers and compose results into a single VerificationStatus
    * @param spec specification axioms/definitions
    * @param hypotheses verified lemmas/assumptions
    * @param goal property/step to be proved
    * @param strat overall abstract strategy to be used for the current step
    * @return Verification summary
    */
  def verify(spec: S, hypotheses: Seq[P], goal: P, strat: VerificationStrategy[S, P]): VerifierStatus[S, P]

  override def compare(that: Verifier[S, P]): Int = this.desc compare that.desc
}
