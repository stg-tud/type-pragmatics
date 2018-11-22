package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Assignments.{generateAssignments, wrapMetaVars}
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

  def selectSuccessPredicate(lemma: Lemma, function: FunctionDef,
                             additionalSuccessVars: Set[MetaVar] = Set()): Seq[Refinement] = {
    val assignments = generateAssignments(lemma, function.inTypes)
    assignments.flatMap(assignment => {
      var freshSuccessVar = FreshVariables.freshMetaVar(
        lemma.freeVariables ++ assignment.toSet,
        function.successfulOutType)
      (additionalSuccessVars + freshSuccessVar).map(successVar =>
        Refinement.SuccessPredicate(function, wrapMetaVars(assignment), successVar)
      )
    })
  }
}
