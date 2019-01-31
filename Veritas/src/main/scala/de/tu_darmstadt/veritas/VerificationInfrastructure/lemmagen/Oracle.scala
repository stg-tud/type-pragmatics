package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.LemmaApplicationStep
import de.tu_darmstadt.veritas.VerificationInfrastructure.{Evidence, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier._
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpFalse
import de.tu_darmstadt.veritas.backend.ast._

import scala.collection.mutable

object Oracle {
  trait Answer
  case class Inconclusive() extends Answer
  case class ProvablyFalse(lemmas: Option[Seq[String]]) extends Answer
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

  def invoke[A <: TypingRule](problem: Problem, lemmas: Set[A], timeout: Integer = 10, logic: String = "tff"): Answer = {
    val falseGoal = Goals(Seq(TypingRule("false-goal", Seq(), Seq(FunctionExpJudgment(FunctionExpFalse)))), None)
    val verifier = new TPTPVampireVerifier(timeout, "4.2.2", logic)
    val assumptions = lemmas.map(lemma => (LemmaApplicationStep(lemma.name), lemma)).toSeq
    // sanity check for distinguishable names
    require(lemmas.map(_.name).size == lemmas.size)
    val result = verifier.verify(
      falseGoal,
      problem.spec,
      Seq(),
      assumptions,
      None,
      stepResultProducer
    )
    result.status match {
      case Finished(Proved(ATPResultDetails(_, _, _, Some(usedLemmas), _)), _) => ProvablyFalse(Some(usedLemmas))
      case Finished(Proved(_), _) => ProvablyFalse(None)
      case _ => Inconclusive()
    }
  }

  def rename(lemmas: Seq[Lemma]): Seq[(Lemma, Lemma)] = {
    for((l, i) <- lemmas.zipWithIndex) yield (l, l.rename(s"Lemma$i"))
  }

  def pruneProvablyFalseLemmas(problem: Problem, lemmas: Set[Lemma]): Set[Lemma] = {
    val remainingLemmas = new mutable.HashSet[Lemma]()
    val lemmaSeq = lemmas.toSeq
    val renamedLemmas = rename(lemmaSeq)
    remainingLemmas ++= renamedLemmas.map(_._2)
    val nameMap = renamedLemmas.map {
      case (original, renamed) => (renamed.name, original)
    }.toMap
    var run = true
    while (run && remainingLemmas.nonEmpty) {
      Oracle.invoke(problem, remainingLemmas.toSet) match {
        case Oracle.Inconclusive() => run = false
        case Oracle.ProvablyFalse(Some(usedLemmas)) =>
          val usedRemainingLemmas = remainingLemmas.filter(lemma => usedLemmas.contains(lemma.name) || usedLemmas.contains(s"'${lemma.name}'"))
          if (usedRemainingLemmas.isEmpty) {
            sys.error("inconsistent spec") // TODO
          } else if (usedRemainingLemmas.size == 1) {
            remainingLemmas.remove(usedRemainingLemmas.head)
          } else {
            sys.error("used more than 1 lemma") // TODO
          }
        }
    }
    remainingLemmas.map(lemma => nameMap(lemma.name)).toSet
  }
}
