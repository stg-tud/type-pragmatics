package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.{GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.backend.ast.{Module, ModuleDef, VeritasConstruct}

/**
  * Verifier for translating Veritas AST to TPTP (with selected transformation strategy) and calling
  * Vampire on the result
  */
class TPTPVampireVerifier(timeout: Int = 10, version: String = "4.1") extends Verifier[VeritasConstruct, VeritasConstruct] {
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
  override def verify[Result <: GenStepResult[VeritasConstruct, VeritasConstruct]]
  (goal: VeritasConstruct,
   spec: VeritasConstruct,
   assumptions: Iterable[VeritasConstruct],
   hints: Option[VerifierHints],
   produce: StepResultProducer[VeritasConstruct, VeritasConstruct, Result]): Result = {
    //TODO add error handling (transformation can fail, prover call can fail etc.)

    val transformer = new VeritasTransformerBestStrat()
    val vampire = Vampire(version, timeout)
    spec match {
      case Module(name, imps, moddefs) => {
        val assmmoddefs = assumptions.toSeq flatMap { s =>
          s match {
            case m: ModuleDef => Seq(m)
            case _ => {
              println(s"Ignored $s since it was not a ModuleDef.");
              Seq()
            }
          }
        }
        val assembledProblemSpec = Module(name + "withassms", imps, moddefs ++ assmmoddefs)

        val transformedProb = transformer.transformProblem(assembledProblemSpec, goal)

        transformedProb match {
          case Left(tptp) => {
            val proverstatus = vampire.callProver(tptp)
            produce.newStepResult(Finished(proverstatus, this),
              proverstatus.proverResult.proofEvidence,
              proverstatus.proverResult.message)
          }
          case Right(err) => {
            produce.newStepResult(Failure(s"Problem during transformation step: $err", this), None, None)
          }
        }
      }
      case _ => produce.newStepResult(Failure(s"Specification was not a module $spec", this), None, None)
    }


  }
}
