package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.Assignments
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.Assignments.wrapMetaVars
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.Constraint.Constraint
import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

trait StrategyHelpers {
  val problem: Problem
  private implicit val enquirer: LemmaGenSpecEnquirer = problem.enquirer
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.queries.Query._

  def selectPredicate(lemma: Lemma, predicate: FunctionDef): Seq[Refinement] = {
    val assignments = Assignments.generateSimple(lemma, predicate.signature.in)
    assignments.map(assignment => Refinement.Predicate(predicate, wrapMetaVars(assignment)))
  }

  def selectPredicate(lemma: Lemma, predicate: FunctionDef, constraints: Seq[Constraint]): Seq[Refinement] = {
    val assignments = Assignments.generate(lemma, constraints)
    assignments.map(assignment => Refinement.Predicate(predicate, wrapMetaVars(assignment)))
  }

  def refine(lemma: Lemma, refinement: Seq[Refinement]): Seq[Lemma] = {
    refinement.flatMap(_.refine(problem, lemma))
  }

  def selectSuccessPredicate(lemma: Lemma, function: FunctionDef,
                             freshSuccessVar: Boolean = true,
                             additionalSuccessVars: Set[MetaVar] = Set()): Seq[Refinement.SuccessPredicate] = {
    val assignments = Assignments.generateSimple(lemma, function.signature.in)
    assignments.flatMap(assignment => {
      var successVars = additionalSuccessVars
      if(freshSuccessVar)
        successVars += FreshVariables.freshMetaVar(
          lemma.boundVariables ++ assignment.toSet,
          function.successfulOutType)
      successVars.map(successVar =>
        Refinement.SuccessPredicate(function, wrapMetaVars(assignment), successVar)
      )
    })
  }

  def selectSuccessPredicate(lemma: Lemma, function: FunctionDef,
                             constraints: Seq[Constraint], successVarConstraint: Constraint): Seq[Refinement.SuccessPredicate] = {
    val query = successVarConstraint +: constraints
    Assignments.generate(lemma, query).map {
      case successVar :: arguments => Refinement.SuccessPredicate(function, wrapMetaVars(arguments), successVar)
    }
  }
}
