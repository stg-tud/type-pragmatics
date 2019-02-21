package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Assignments.wrapMetaVars
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Constraint.Constraint
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

trait StrategyHelpers {
  val problem: Problem
  private implicit val enquirer: LemmaGenSpecEnquirer = problem.enquirer
  import Query._

  def selectPredicate(lemma: Lemma, predicate: FunctionDef): Seq[Refinement.Predicate] = {
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

  def selectSuccessfulApplication(lemma: Lemma, function: FunctionDef): Seq[Refinement.SuccessfulApplication] = {
    selectSuccessfulApplication(lemma, function,
      Constraint.preferBound(function.inTypes),
      Constraint.fresh(function.successfulOutType))
  }

  def selectSuccessfulApplication(lemma: Lemma, function: FunctionDef,
                                  constraints: Seq[Constraint], successVarConstraint: Constraint): Seq[Refinement.SuccessfulApplication] = {
    val query = successVarConstraint +: constraints
    Assignments.generate(query, lemma).map {
      case successVar :: arguments => Refinement.SuccessfulApplication(function, wrapMetaVars(arguments), successVar)
    }
  }
}
