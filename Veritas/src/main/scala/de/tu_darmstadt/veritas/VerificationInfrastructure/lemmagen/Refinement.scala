package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar, VeritasConstruct}
import de.tu_darmstadt.veritas.backend.ast.function._

trait Refinement {
  def refine(problem: Problem, lemma: Lemma): Option[Lemma]
}

object Refinement {
  case class SuccessfulApplication(function: FunctionDef,
                                   arguments: Seq[FunctionExpMeta],
                                   result: MetaVar) extends Refinement {
    def refine(problem: Problem, lemma: Lemma): Option[Lemma] = {
      val invocationExp = FunctionExpApp(
        function.signature.name,
        arguments
      )
      var right: FunctionExpMeta = FunctionMeta(result)
      if(problem.enquirer.isFailableType(function.signature.out)) {
        val (_, successConstructor) = problem.enquirer.retrieveFailableConstructors(function.signature.out)
        right = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(result)))
      }
      val equality = problem.enquirer.makeEquation(invocationExp, right).asInstanceOf[FunctionExpJudgment]
      if(lemma.premises.contains(equality) || lemma.consequences.contains(equality)) {
        None
      } else {
        val leftSides = lemma.premises.collect {
          case FunctionExpJudgment(FunctionExpEq(left, _)) => left
        }
        if(leftSides.contains(invocationExp)) {
          None
        } else {
          Some(lemma.addPremise(equality))
        }
      }
    }

    def refineNeg(problem: Problem, lemma: Lemma): Option[Lemma] = {
      val invocationExp = FunctionExpApp(
        function.signature.name,
        arguments
      )
      var right: FunctionExpMeta = FunctionMeta(result)
      if(problem.enquirer.isFailableType(function.signature.out)) {
        val (_, successConstructor) = problem.enquirer.retrieveFailableConstructors(function.signature.out)
        right = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(result)))
      }
      val equality = problem.enquirer.makeInequation(invocationExp, right).asInstanceOf[FunctionExpJudgment]
      if(lemma.premises.contains(equality) || lemma.consequences.contains(equality)) {
        None
      } else {
        val leftSides = lemma.premises.collect {
          case FunctionExpJudgment(FunctionExpEq(left, _)) => left
        }
        if(leftSides.contains(invocationExp)) {
          None
        } else {
          Some(lemma.addPremise(equality))
        }
      }
    }

    override def toString: String = s"SuccessfulApplication(${function.signature.name}, $arguments, $result)"
  }

  case class Predicate(predicate: FunctionDef,
                       arguments: Seq[FunctionExpMeta]) extends Refinement {
    def refine(problem: Problem, lemma: Lemma): Option[Lemma] = {
      val invocationExp = FunctionExpJudgment(FunctionExpApp(predicate.signature.name, arguments))
      if(lemma.premises.contains(invocationExp) || lemma.consequences.contains(invocationExp)) {
        None
      } else {
        Some(lemma.addPremise(invocationExp))
      }
    }

    override def toString: String = s"Predicate(${predicate.signature.name}, $arguments)"
  }

  case class Equation(left: MetaVar, right: MetaVar) extends Refinement {
    def refine(problem: Problem, lemma: Lemma): Option[Lemma] = {
      // we can just rename it to the left side
      Some(Lemma.fromTypingRule(LemmaEquivalence.renameVariables(lemma, { mv =>
        if(mv == right)
          left
        else
          mv
      })))
    }
  }

}
