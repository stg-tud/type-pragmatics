package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.Assignments.wrapMetaVars
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.Constraint.Constraint
import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

trait StrategyHelpers {
  val problem: Problem
  private implicit val enquirer: LemmaGenSpecEnquirer = problem.enquirer
  import Query._

  def selectPredicate(lemma: Lemma, predicate: FunctionDef): Seq[Refinement] = {
    val assignments = Assignments.generateSimple(predicate.signature.in, lemma)
    assignments.map(assignment => Refinement.Predicate(predicate, wrapMetaVars(assignment)))
  }

  def selectPredicate(lemma: Lemma, predicate: FunctionDef, constraints: Seq[Constraint]): Seq[Refinement] = {
    val assignments = Assignments.generate(constraints, lemma)
    assignments.map(assignment => Refinement.Predicate(predicate, wrapMetaVars(assignment)))
  }

  def refine(lemma: Lemma, refinement: Seq[Refinement]): Seq[Lemma] = {
    refinement.flatMap(_.refine(problem, lemma))
  }

  def selectSuccessPredicate(lemma: Lemma, function: FunctionDef): Seq[Refinement.SuccessPredicate] = {
    selectSuccessPredicate(lemma, function,
      Constraint.preferBound(function.inTypes),
      Constraint.fresh(function.successfulOutType))
  }

  def selectSuccessPredicate(lemma: Lemma, function: FunctionDef,
                             constraints: Seq[Constraint], successVarConstraint: Constraint): Seq[Refinement.SuccessPredicate] = {
    val query = successVarConstraint +: constraints
    Assignments.generate(query, lemma).map {
      case successVar :: arguments => Refinement.SuccessPredicate(function, wrapMetaVars(arguments), successVar)
    }
  }
}
