package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.backend.ast.{Module, VeritasConstruct}

import scala.util.{Failure, Success}

/**
  * Created by andiderp on 06/04/2017.
  */
class TPTPEproverVerifier(timeout: Int = 10) extends TPTPVerifier {
  /** Textual description that should be unique (used for ordering verifiers) */
  override val desc: String = "TPTPEproverVerifier"

  override def prover: Prover[TPTP] = Eprover(timeout)
}

trait TPTPVerifier extends Verifier[VeritasConstruct, VeritasConstruct] {
  override type V = TPTP

  def prover: Prover[TPTP]

  override def verify[Result <: GenStepResult[VeritasConstruct, VeritasConstruct]](goal: VeritasConstruct, spec: VeritasConstruct, parentedges: Iterable[EdgeLabel], assumptions: Iterable[VeritasConstruct], hints: Option[VerifierHints], produce: StepResultProducer[VeritasConstruct, VeritasConstruct, Result]): Result = {
    val transformer = VeritasTransformerBestStrat
    spec match {
      case Module(name, imps, moddefs) => {
        val transformedProb = transformer.transformProblem(goal, spec, parentedges, assumptions)

        transformedProb match {
          case Success(tptp) => {
            val proverstatus = prover.callProver(tptp)
            produce.newStepResult(Finished(proverstatus, this),
              proverstatus.proverResult.proofEvidence,
              proverstatus.proverResult.message)
          }
          case Failure(err) => {
            produce.newStepResult(VerifierFailure(s"Problem during transformation step: $err", this), None, None)
          }
        }
      }
      case _ => produce.newStepResult(VerifierFailure(s"Specification was not a module $spec", this), None, None)
    }
  }
}
