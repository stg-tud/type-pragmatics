package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar, VeritasConstruct}
import de.tu_darmstadt.veritas.backend.ast.function._

/** A refinement encapsulates a syntactic modification of an input lemma. */
trait Refinement {
  def refine(problem: Problem, lemma: Lemma): Option[Lemma]
}

/** This object defines three refinements Predicate, SuccessfulApplication and Equation. */
object Refinement {
  case class SuccessfulApplication(function: FunctionDef,
                                   arguments: Seq[FunctionExpMeta],
                                   result: MetaVar) extends Refinement {
    def refine(problem: Problem, lemma: Lemma): Option[Lemma] = {
      val invocationExp = FunctionExpApp(
        function.signature.name,
        arguments
      )
      // add success constructor if needed
      var right: FunctionExpMeta = FunctionMeta(result)
      if(problem.enquirer.isFailableType(function.signature.out)) {
        val (_, successConstructor) = problem.enquirer.retrieveFailableConstructors(function.signature.out)
        right = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(result)))
      }
      // make equality AST
      val equality = problem.enquirer.makeEquation(invocationExp, right).asInstanceOf[FunctionExpJudgment]
      // check that the premises and consequences do not already contain this exact equality
      if(lemma.premises.contains(equality) || lemma.consequences.contains(equality)) {
        None
      } else {
        // check that no left side of a premise contains this exact equality
        val leftSides = lemma.premises.collect {
          case FunctionExpJudgment(FunctionExpEq(left, _)) => left
        }
        if(leftSides.contains(invocationExp)) {
          None
        } else {
          // apply the refinement
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
      // check that no premise or consequence contains this exact equality yet
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
      // check that both variables are bound by lemma
      if(Set(left, right) subsetOf lemma.boundVariables) {
        Some(Lemma.fromTypingRule(LemmaEquivalence.renameVariables(lemma, { mv =>
          if (mv == right)
            left
          else
            mv
        })))
      } else {
        None
      }
    }
  }

}
