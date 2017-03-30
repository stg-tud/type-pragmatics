package de.tu_darmstadt.veritas

import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier._

/**
  * Created by andiderp on 27/03/2017.
  */
package object VerificationInfrastructure {

  // Mock classes for testing
  case class MockAlwaysVerifier[Spec, Goal]() extends Verifier[Spec, Goal] {

    class MyV extends VerifierFormat

    override type V = MyV
    /** Textual description that should be unique (used for ordering verifiers) */
    override val desc: String = "I_always_verify_everything"

    /**
      * A concrete verifier may call any combination of transformers & provers
      * (or do something else to produce a verification result)
      *
      * @param goal
      * @param spec
      * @param lassumptions
      * @param hints
      * @param produce
      * @tparam Result
      * @return
      */
    override def verify[Result <: GenStepResult[Spec, Goal]](goal: Goal, spec: Spec,
                                                             lassumptions: Iterable[(Goal, EdgeLabel)],
                                                             hints: Option[VerifierHints],
                                                             produce: StepResultProducer[Spec, Goal, Result]): Result =
      produce.newStepResult(Finished[Spec, Goal](Proved(ATPResultDetails("no log", None)), this), None, None)
  }

}
