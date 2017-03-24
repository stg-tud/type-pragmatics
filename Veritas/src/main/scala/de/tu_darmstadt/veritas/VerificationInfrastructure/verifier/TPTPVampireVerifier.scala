package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.{GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.backend.ast.VeritasConstruct

/**
  * Verifier for translating Veritas AST to TPTP (with selected transformation strategy) and calling
  * Vampire on the result
  */
class TPTPVampireVerifier extends Verifier[Seq[VeritasConstruct], VeritasConstruct] {
  override type V = TPTP
  /** Textual description that should be unique (used for ordering verifiers) */
  override val desc: String = "TPTPVampireVerifier"

  /**
    * A concrete verifier may call any combination of transformers & provers
    * (or do something else to produce a verification result)
    *
    * verify may take hints from the caller (i.e. the tactic;
    * a hint could for example be an induction scheme, prover timeout etc.
    *
    * //TODO maybe make VerifierHints less general?
    * //TODO maybe introduce a mechanism for letting a Verifier "announce" to a caller what hints it requires?
    *
    * this implementation does not require any hints for verification
    *
    * @param goal
    * @param spec
    * @param assumptions
    * @param hints
    * @param produce
    * @tparam Result
    * @return
    */
  override def verify[Result <: GenStepResult[Seq[VeritasConstruct], VeritasConstruct]]
  (goal: VeritasConstruct,
   spec: Seq[VeritasConstruct],
   assumptions: Iterable[VeritasConstruct],
   hints: Option[VerifierHints],
   produce: StepResultProducer[Seq[VeritasConstruct], VeritasConstruct, Result]): Result = {
    //TODO add error handling (transformation can fail, prover call can fail etc.)

    val transformer = new VeritasTransformerBestStrat()
    //TODO: make timeout into parameter for this concrete verifier?
    val vampire = Vampire("4.1", 10)
    val assembledProblemSpec = spec ++ assumptions.toSeq
    val transformedProb = transformer.transformProblem(assembledProblemSpec, goal)

    val proverstatus = vampire.callProver(transformedProb)

    //TODO refine production of step result; add evidence
    produce.newStepResult(Finished(proverstatus, this), None, None)

  }
}
