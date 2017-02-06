package de.tu_darmstadt.veritas.VerificationInfrastructure

import scala.collection.GenSeq



/**
  * Verifiers "manage" verification attempts (i.e. compiling the problem, calling one or more provers,
  * starting/stopping a proof attempt...)
  *
  * //TODO should this rather be a concrete case class?
  */
abstract class Verifier[S, P] {
  type V //Representation of Verification format
  val transformer : GenSeq[Transformer[S, P, V]] //translate a given specification (S) + goal (P) to a format for verification (V)
  val provers : GenSeq[Prover[V]] //sequence of provers that understand the given verification format (may be called in parallel)
  val supportedStrategies: Seq[VerificationStrategy] //general verification strategies that the provers can be called with
  // TODO: compute intersection of strategies that are supported

  //TODO: calls to prover can fail - maybe include exception propagation
  /**
    * combine calling all transformers with all provers and compose results into a single VerificationStatus
    * @param spec specification axioms/definitions
    * @param hypotheses verified lemmas/assumptions
    * @param goal property/step to be proved
    * @param strat overall abstract strategy to be used for the current step
    * @return Verification summary
    */
  def verify(spec: S, hypotheses: Seq[P], goal: P, strat: VerificationStrategy): VerificationStatus

}
