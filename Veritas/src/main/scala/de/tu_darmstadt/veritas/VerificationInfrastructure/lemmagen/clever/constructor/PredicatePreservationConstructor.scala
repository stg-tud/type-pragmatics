package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.constructor

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.{AnnotatedLemma, Hints, RefinementNode}
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar, SortRef}


class PredicatePreservationConstructor(val problem: Problem,
                                       function: FunctionDef,
                                       predicate: FunctionDef,
                                       val hints: Hints) extends LemmaGraphConstructor {
  import Query._
  implicit private val enquirer: LemmaGenSpecEnquirer = problem.enquirer

  def termType: SortRef = function.successfulOutType

  def generatePredicateArguments(fixedArg: MetaVar): Seq[MetaVar] = {
    val constraints = predicate.inTypes.map(inType =>
      if(inType == fixedArg.sortType)
        Constraint.fixed(fixedArg)
      else
        Constraint.fresh(inType)
    )
    Assignments.generate(constraints).head // TODO
  }

  private val resultVar :: functionArgs = Assignments.generateSimpleSingle(termType +: function.inTypes)
  private val predicateArgs = generatePredicateArguments(resultVar)

  override def invocationArguments: Seq[MetaVar] = functionArgs
  override def restrictableVariables(node: RefinementNode): Set[MetaVar] = {
    node.lemma.boundVariables.filterNot(_.sortType == termType)
  }

  def generateBase(): AnnotatedLemma = {
    // --------------------
    // [predicate]([], ...)
    // [producer]([], ...) =  []
    // producer arguments can be fresh or bound with matching types
    // the success variable can be any of the arguments of ``predicate``, with matching types
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${function.name}Preservation${predicate.name}", Seq(), Seq(judgment))
    // we find all inVars with matching type
    val matchingInVars = functionArgs.filter(_.sortType == termType)
    // for each matching in var, add a Predicate refinement
    var lemma = baseLemma
    val r = Refinement.SuccessfulApplication(function, Assignments.wrapMetaVars(functionArgs), resultVar)
    var constrainedVars = predicateArgs.toSet
    lemma = r.refine(problem, lemma).getOrElse(lemma)
    for(inVar <- matchingInVars) {
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
    AnnotatedLemma(lemma, constrainedVars, predicateArgs.toSet)
  }
}
