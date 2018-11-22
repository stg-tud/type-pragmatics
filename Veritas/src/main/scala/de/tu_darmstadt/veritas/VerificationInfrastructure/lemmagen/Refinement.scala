package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Assignments.{generateAssignments, wrapMetaVars}
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.VeritasSpecEnquirer
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionExpMeta, FunctionMeta}

trait Refinement {
  def refine(problem: Problem, lemma: Lemma): Lemma
}

object Refinement {
  case class SuccessPredicate(function: FunctionDef,
                              arguments: Seq[FunctionExpMeta],
                              result: MetaVar) extends Refinement {
    def refine(problem: Problem, lemma: Lemma): Lemma = {
      val (_, successConstructor) = problem.enquirer.retrieveFailableConstructors(function.signature.out)
      val invocationExp = FunctionExpApp(
        function.signature.name,
        arguments
      )
      val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(result)))
      val equality = problem.enquirer.makeEquation(invocationExp, successExp).asInstanceOf[FunctionExpJudgment]
      if(lemma.premises.contains(equality)) {
        lemma
      } else {
        lemma.addPremise(equality)
      }
    }
  }

  case class Predicate(predicate: FunctionDef,
                       arguments: Seq[FunctionExpMeta]) extends Refinement {
    def refine(problem: Problem, lemma: Lemma): Lemma = {
      val invocationExp = FunctionExpJudgment(FunctionExpApp(predicate.signature.name, arguments))
      if(lemma.premises.contains(invocationExp)) {
        lemma
      } else {
        lemma.addPremise(invocationExp)
      }
    }
  }


}
