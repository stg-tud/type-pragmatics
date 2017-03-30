package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenStepResult, StepResultProducer}

trait VerifierFormat

trait VerifierHints

/**
  * Verifiers "manage" verification attempts (i.e. compiling the problem, calling one or more provers,
  * starting/stopping a proof attempt...)
  *
  */
trait Verifier[Spec, Goal] extends Ordered[Verifier[Spec, Goal]] with Serializable {
  type V <: VerifierFormat

  /** Textual description that should be unique (used for ordering verifiers) */
  val desc: String

  override def compare(that: Verifier[Spec, Goal]): Int = this.desc compare that.desc

  /**
    * A concrete verifier may call any combination of transformers & provers
    * (or do something else to produce a verification result)
    *
    * Edge Labels from proof graphs can contain hints for verification (e.g. fixed variables, induction scheme
    * verify may take hints from the caller (i.e. the tactic;
    * a hint could for example be an induction scheme, prover timeout etc.
    * etc.). A concrete verifier can define how to use these hints when verifying.
    *
    *
    * //TODO maybe make VerifierHints less general?
    * //TODO maybe introduce a mechanism for letting a Verifier "announce" to a caller what hints it requires?
    *
    * @param goal
    * @param spec
    * @param lassumptions
    * @param hints
    * @param produce
    * @tparam Result
    * @return
    */
  def verify[Result <: GenStepResult[Spec, Goal]](goal: Goal, spec: Spec,
                                                  lassumptions: Iterable[(Goal, EdgeLabel)],
                                                  hints: Option[VerifierHints],
                                                  produce: StepResultProducer[Spec, Goal, Result]): Result

}
