package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem, Query, Refinement}
import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionMeta}

class PreservationGenerator(val problem: Problem, function: FunctionDef, predicate: FunctionDef) {
  import Query._

  implicit private val enquirer = problem.enquirer

  def generatePredicateArguments(fixedArg: MetaVar): Seq[MetaVar] = {
    val constraints = predicate.inTypes.map(inType =>
      if(inType == fixedArg.sortType)
        Constraint.fixed(fixedArg)
      else
        Constraint.fresh(inType)
    )
    Assignments.generate(constraints).head // TODO
  }

  def generateBase(): Lemma = {
    // --------------------
    // [predicate]([], ...)
    val outType = function.successfulOutType
    /*
    val predicateArgs = Assignments.generateSimpleSingle(predicate.inTypes)
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${function.name}${predicate.name}Preservation", Seq(), Seq(judgment))*/
    // [producer]([], ...) =  []
    // producer arguments can be fresh or bound with matching types
    val outVar :: inVars = Assignments.generateSimpleSingle(outType +: function.inTypes)
    // the success variable can be any of the arguments of ``predicate``, with matching types
    println(outVar)
    println(inVars)
    val predicateArgs = generatePredicateArguments(outVar)
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${function.name}${predicate.name}Preservation", Seq(), Seq(judgment))
    // we find all inVars with matching type
    val matchingInVars = inVars.filter(_.sortType == outType)
    // for each matching in var, add a Predicate refinement
    var lemma = baseLemma
    val r = Refinement.SuccessfulApplication(function, Assignments.wrapMetaVars(inVars), outVar)
    lemma = r.refine(problem, lemma).getOrElse(lemma)
    for(inVar <- matchingInVars) {
      val constraints = predicate.inTypes.map(inType =>
        if(inType == inVar.sortType)
          Constraint.fixed(inVar)
        else
          /*Constraint.fresh(inType)*/
          Constraint.preferBound(inType)
      )
      val assignment = Assignments.generate(constraints, lemma).head
      val refinement = Refinement.Predicate(predicate, Assignments.wrapMetaVars(assignment))
      lemma = refinement.refine(problem, lemma).getOrElse(lemma)
    }
    lemma
    /*
    val matchingPredicateArgs = predicateArgs.filter(_.sortType == outType)
    val successVarConstraint = Constraint.Union(matchingPredicateArgs.map(Constraint.Fixed).toSet)
    // we do not need refinements that postulate that the producer returns its argument
    val baseLemmas = refine(baseLemma,
      selectSuccessfulApplication(baseLemma, function, producerArgumentsConstraints, successVarConstraint)
        .filterNot(r => r.arguments contains FunctionMeta(r.result)))
    // [predicate]([], ...)
    val evolvedLemmas = baseLemmas.flatMap(lemma => {
      val constraints = Constraint.freshOrBound(predicate.inTypes)
      refine(lemma, selectPredicate(lemma, predicate, constraints))
    })*/
  }
}
