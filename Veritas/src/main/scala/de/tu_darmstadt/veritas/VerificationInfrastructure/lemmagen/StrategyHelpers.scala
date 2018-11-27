package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Assignments.{Placement, generateAssignments, wrapMetaVars}
import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

trait StrategyHelpers {
  val problem: Problem
  private implicit val enquirer: LemmaGenSpecEnquirer = problem.enquirer
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.queries.Query._

  def selectPredicate(lemma: Lemma, predicate: FunctionDef): Seq[Refinement] = {
    val assignments = generateAssignments(lemma, predicate.signature.in)
    assignments.map(assignment => Refinement.Predicate(predicate, wrapMetaVars(assignment)))
  }

  def refine(lemma: Lemma, refinement: Seq[Refinement]): Seq[Lemma] = {
    refinement.flatMap(_.refine(problem, lemma))
  }

  def selectSuccessPredicate(lemma: Lemma, function: FunctionDef,
                             freshSuccessVar: Boolean = true,
                             additionalSuccessVars: Set[MetaVar] = Set()): Seq[Refinement.SuccessPredicate] = {
    val assignments = generateAssignments(lemma, function.inTypes)
    assignments.flatMap(assignment => {
      var successVars = additionalSuccessVars
      if(freshSuccessVar)
        successVars += FreshVariables.freshMetaVar(
          lemma.freeVariables ++ assignment.toSet,
          function.successfulOutType)
      successVars.map(successVar =>
        Refinement.SuccessPredicate(function, wrapMetaVars(assignment), successVar)
      )
    })
  }

  def selectSuccessPredicate(lemma: Lemma, function: FunctionDef,
                             placements: Seq[Placement], successVar: Placement): Seq[Refinement.SuccessPredicate] = {
    val argumentAssignments = Assignments.placeVariables(lemma, placements)
    argumentAssignments.flatMap(assignment => {
      val successVars = Assignments.generatePlacementChoice(lemma, successVar, Seq(), assignment.toSet)
      successVars.map(successVar =>
        Refinement.SuccessPredicate(function, wrapMetaVars(assignment), successVar)
      )
    })
  }
}
