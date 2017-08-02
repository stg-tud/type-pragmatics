package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast.{Module, VeritasConstruct}

import scala.util.{Failure, Success}

/**
  * Created by andiderp on 10.07.17.
  */
trait SMTLibVerifier extends Verifier[VeritasConstruct, VeritasConstruct] {
  override type V = SMTLibFormat

  def prover: Prover[SMTLibFormat]

  override def verify[Result <: GenStepResult[VeritasConstruct, VeritasConstruct]](goal: VeritasConstruct, spec: VeritasConstruct, parentedges: Iterable[EdgeLabel], assumptions: Iterable[VeritasConstruct], hints: Option[VerifierHints], produce: StepResultProducer[VeritasConstruct, VeritasConstruct, Result]): Result = {
    val transformer = new VeritasTransformer[SMTLibFormat](
      Configuration(Map(FinalEncoding -> FinalEncoding.SMTLib,
        Simplification -> Simplification.LogicalAndConstructors,
        VariableEncoding -> VariableEncoding.InlineEverything,
        Selection -> Selection.SelectAll,
        Problem -> Problem.All)), x => x.asInstanceOf[SMTLibFormat])
    spec match {
      case Module(name, imps, moddefs) => {
        val transformedProb = transformer.transformProblem(goal, spec, parentedges, assumptions)

        transformedProb match {
          case Success(smtLib) => {
            //for debugging purposes
            //println("SMTLIB file: ")
            //println(smtLib.toString)
            val proverstatus = prover.callProver(smtLib)
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
