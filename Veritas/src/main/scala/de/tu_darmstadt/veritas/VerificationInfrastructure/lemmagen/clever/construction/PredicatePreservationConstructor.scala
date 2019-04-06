package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.{AnnotatedLemma, Hints, RefinementNode}
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar, SortRef}


/** Graph constructor for predicate preservation refinement graphs. */
class PredicatePreservationConstructor(val problem: Problem,
                                       function: FunctionDef,
                                       predicate: FunctionDef,
                                       val hints: Hints) extends LemmaGraphConstructor {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Query._
  implicit private val enquirer: LemmaGenSpecEnquirer = problem.enquirer

  def termType: SortRef = function.successfulOutType

  def generatePredicateArguments(fixedArg: MetaVar): Seq[MetaVar] = {
    val constraints = predicate.inTypes.map(inType =>
      if(inType == fixedArg.sortType)
        Constraint.fixed(fixedArg)
      else
        Constraint.fresh(inType)
    )
    Assignments.generate(constraints).head
  }

  // generate the successful result variable and the arguments for the invocation of `function`
  private val resultVar :: functionArgs = Assignments.generateSimpleSingle(termType +: function.inTypes)
  // generate the predicate arguments: these are fresh variable symbols, but we replace
  // the sole variable of sort `termType` with `resultVar`.
  private val predicateArgs = generatePredicateArguments(resultVar)

  override def invocationArguments: Seq[MetaVar] = functionArgs
  override def restrictableVariables(node: RefinementNode): Set[MetaVar] = {
    node.lemma.boundVariables.filterNot(_.sortType == termType)
  }

  def generateBase(): AnnotatedLemma = {
    // we first generate the consequence, which contains an invocation of `predicate`.
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${function.name}Preservation${predicate.name}", Seq(), Seq(judgment))
    // we add a premise that postulates a successful application of `function`
    val functionApplicationRefinement =
      Refinement.SuccessfulApplication(function, Assignments.wrapMetaVars(functionArgs), resultVar)
    var lemma = functionApplicationRefinement.refine(problem, lemma).getOrElse(lemma)
    // iteratively build the set of constrained variables
    var constrainedVars = predicateArgs.toSet
    // we find all arguments of `function` with type `termType`.
    val matchingInVars = functionArgs.filter(_.sortType == termType)
    // for each such argument, we add one premise
    for(inVar <- matchingInVars) {
      // generate fresh variables, but pass `inVar` to the predicate
      val constraints = predicate.inTypes.map(inType =>
        if(inType == inVar.sortType)
          Constraint.fixed(inVar)
        else
          Constraint.fresh(inType)
      )
      val assignment = Assignments.generate(constraints, lemma).head
      val refinement = Refinement.Predicate(predicate, Assignments.wrapMetaVars(assignment))
      constrainedVars ++= assignment.toSet
      lemma = refinement.refine(problem, lemma).getOrElse(lemma)
    }
    // generate the base lemma
    AnnotatedLemma(lemma, constrainedVars, predicateArgs.toSet)
  }
}
