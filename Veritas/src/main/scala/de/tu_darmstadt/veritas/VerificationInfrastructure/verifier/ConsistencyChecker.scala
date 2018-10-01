package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.Configuration.Problem
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpFalse
import de.tu_darmstadt.veritas.backend.ast._

import scala.util.{Failure, Success, Try}

case class ConsistencyChecker[Format <: VerifierFormat](config: Configuration, prover: Prover[Format]) {

  def consistent(spec: Module): Try[Boolean] = {
    val updatedConfig = config.m.updated(Problem, Problem.Consistency)
    val transformer = new VeritasTransformer[Format](Configuration(updatedConfig), _.asInstanceOf[Format])
    val problem = transformer.transformProblem(
      Goals(Seq(TypingRule("false-goal", Seq(), Seq(FunctionExpJudgment(FunctionExpFalse)))), None), spec, Seq(), Seq())
    problem match {
      case Success(result) =>
        prover.callProver(result) match {
          case Inconclusive(_) => Success(true)
          case Proved(rd) => {
            println(rd.proofEvidence.get)
            Success(false)
          }
          case _ => Success(false)
        }
      case Failure(fail) => fail match {
        case FormatTypeInferenceError(e) => Failure(e)
        case _ => Failure(fail)
      }
    }
  }
}
