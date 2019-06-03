package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Assignments.wrapMetaVars
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Constraint.Constraint
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

/** Helper methods for refinement strategies. */
trait StrategyHelpers {
  val problem: Problem
  private implicit val enquirer: LemmaGenSpecEnquirer = problem.enquirer
  import Query._

  /** Return a sequence of refinements that add the predicate `predicate` to the premises of `lemma`.
    * For every argument of `predicate`, we prefer to pass bound variables of `lemma`. If there
    * is no bound variable of according type, generate a fresh variable symbol.
    */
  def selectPredicate(lemma: Lemma, predicate: FunctionDef): Seq[Refinement.Predicate] = {
    val assignments = Assignments.generateSimple(predicate.signature.in, lemma)
    assignments.map(assignment => Refinement.Predicate(predicate, wrapMetaVars(assignment)))
  }

  /** Return a sequence of refinements that add the predicate `predicate` to the premises of `lemma`.
    * Return all refinements whose choice of arguments fulfills the constraints `constraints`.
    */
  def selectPredicate(lemma: Lemma, predicate: FunctionDef, constraints: Seq[Constraint]): Seq[Refinement] = {
    val assignments = Assignments.generate(constraints, lemma)
    assignments.map(assignment => Refinement.Predicate(predicate, wrapMetaVars(assignment)))
  }

  /** Apply all refinements in `refinement` to `lemma`, return a sequence of lemmas. */
  def refine(lemma: Lemma, refinement: Seq[Refinement]): Seq[Lemma] = {
    refinement.flatMap(_.refine(problem, lemma))
  }

  /** Return a sequence of refinements that add a successful application of `function` to the premises of `lemma`.
    * For every argument of `function`, prefer to pass bound variables of `lemma`. If there is no bound
    * variable of according type, generate a fresh variable symbol. Generate a fresh variable symbol for
    * the success variable.
    */
  def selectSuccessfulApplication(lemma: Lemma, function: FunctionDef): Seq[Refinement.SuccessfulApplication] = {
    selectSuccessfulApplication(lemma, function,
      Constraint.preferBound(function.inTypes),
      Constraint.fresh(function.successfulOutType))
  }

  /** Return a sequence of refinements that add a successful application of `function` to the premises of `lemma`.
    * Return all refinements whose choice of arguments fulfills the constraints `constraint`
    * and whose success variable fulfills the constraint `successVarConstraint`.
    */
  def selectSuccessfulApplication(lemma: Lemma, function: FunctionDef,
                                  constraints: Seq[Constraint],
                                  successVarConstraint: Constraint): Seq[Refinement.SuccessfulApplication] = {
    val query = successVarConstraint +: constraints
    Assignments.generate(query, lemma).map {
      case successVar :: arguments => Refinement.SuccessfulApplication(function, wrapMetaVars(arguments), successVar)
    }
  }
}
