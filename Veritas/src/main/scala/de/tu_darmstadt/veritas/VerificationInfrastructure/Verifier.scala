package de.tu_darmstadt.veritas.VerificationInfrastructure

import scala.collection.GenSeq



/**
  * Verifiers "manage" verification attempts (i.e. compiling the problem, calling one or more provers,
  * starting/stopping a proof attempt...)
  */
abstract class Verifier[S, P] {
  type V //Representation of Verification format
  val transformer : Transformer[S, P, V] //translate a given specification (S) + goal (P) to a format for verification (V)
  val provers : GenSeq[Prover[V]] //sequence of provers that understand the given verification format (may be called in parallel)
  val supportedStrategies: Seq[VerificationStrategy] //general verification strategies that the provers can be called with

  //TODO: calls to prover can fail - maybe include exception propagation
  def verify(spec: S, hypotheses: Seq[P], goal: P, strat: VerificationStrategy): ProverStatus

}
