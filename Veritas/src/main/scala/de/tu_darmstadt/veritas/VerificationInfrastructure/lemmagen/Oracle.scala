package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.LemmaApplicationStep
import de.tu_darmstadt.veritas.VerificationInfrastructure.{Evidence, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier._
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpFalse
import de.tu_darmstadt.veritas.backend.ast._

object Oracle {
  trait Answer
  case class Inconclusive() extends Answer
  case class ProvablyFalse() extends Answer
  case class ProvablyTrue() extends Answer

  class StepResult(val status: VerifierStatus[VeritasConstruct, VeritasFormula],
                   val evidence: Option[Evidence], val errorMsg: Option[String])
    extends GenStepResult[VeritasConstruct, VeritasFormula]  {
  }

  object stepResultProducer extends StepResultProducer[VeritasConstruct, VeritasFormula, StepResult] {
    override def newStepResult(status: VerifierStatus[VeritasConstruct, VeritasFormula], evidence: Option[Evidence],
                               errorMsg: Option[String]): StepResult = {
      new StepResult(status, evidence, errorMsg)
    }
  }

  def invoke(problem: Problem, lemma: TypingRule, timeout: Integer = 20, logic: String = "tff"): Answer = {
    val falseGoal = Goals(Seq(TypingRule("false-goal", Seq(), Seq(FunctionExpJudgment(FunctionExpFalse)))), None)
    val verifier = new TPTPVampireVerifier(timeout, "4.1", logic)
    val assumptionLabel = LemmaApplicationStep(lemma.name)
    val result = verifier.verify(
      falseGoal,
      problem.spec,
      Seq(),
      Seq((assumptionLabel, lemma)),
      None,
      stepResultProducer
    )
    //println(result.status)
    result.status match {
      case Finished(Proved(_), _) => ProvablyFalse()
      case _ => Inconclusive()
    }
  }
}
