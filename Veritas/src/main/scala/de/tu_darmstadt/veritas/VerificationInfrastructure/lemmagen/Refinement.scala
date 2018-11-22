package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar}
import de.tu_darmstadt.veritas.backend.ast.function._

trait Refinement {
  def refine(problem: Problem, lemma: Lemma): Option[Lemma]
}

object Refinement {
  case class SuccessPredicate(function: FunctionDef,
                              arguments: Seq[FunctionExpMeta],
                              result: MetaVar) extends Refinement {
    def refine(problem: Problem, lemma: Lemma): Option[Lemma] = {
      val (_, successConstructor) = problem.enquirer.retrieveFailableConstructors(function.signature.out)
      val invocationExp = FunctionExpApp(
        function.signature.name,
        arguments
      )
      val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(result)))
      val equality = problem.enquirer.makeEquation(invocationExp, successExp).asInstanceOf[FunctionExpJudgment]
      if(lemma.premises.contains(equality) || lemma.consequences.contains(equality)) {
        None
      } else {
        val leftSides = lemma.premises.collect {
          case FunctionExpJudgment(FunctionExpEq(left, _)) => left
        }
        if(leftSides.contains(invocationExp)) {
          None
        } else {
          Some(lemma.addPremise(this, equality))
        }
      }
    }

    override def toString: String = s"SuccessPredicate(${function.signature.name}, $arguments, $result)"
  }

  case class Predicate(predicate: FunctionDef,
                       arguments: Seq[FunctionExpMeta]) extends Refinement {
    def refine(problem: Problem, lemma: Lemma): Option[Lemma] = {
      val invocationExp = FunctionExpJudgment(FunctionExpApp(predicate.signature.name, arguments))
      if(lemma.premises.contains(invocationExp) || lemma.consequences.contains(invocationExp)) {
        None
      } else {
        Some(lemma.addPremise(this, invocationExp))
      }
    }

    override def toString: String = s"Predicate(${predicate.signature.name}, $arguments)"
  }


}
