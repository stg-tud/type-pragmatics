package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.{Assignments, Constraint, StrategyHelpers}
import de.tu_darmstadt.veritas.backend.ast.FunctionExpJudgment
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionMeta}

import scala.collection.mutable

/** Refinement Strategy for generating predicate and relational preservation for a specific function */
class PreservationStrategy(override val problem: Problem, function: FunctionDef)
  extends RefinementStrategy with StrategyHelpers {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Query._

  implicit private val enquirer: LemmaGenSpecEnquirer = problem.enquirer

  /** Build a sequence of base relational preservation lemmas for `relation`. */
  def buildRelationalPreservationLemmas(relation: FunctionDef): Seq[Lemma] = {
    // build preservation lemma that postulates that `relation` holds for
    // the original term and for the reduction step
    // -------------------------
    // [relation]([t_1], [t_2])
    val relationArgs = Assignments.generateSimpleSingle(relation.inTypes)
    val Seq(termVar, reducedTermVar) = relationArgs
    val invocationExp = FunctionExpApp(relation.name, Assignments.wrapMetaVars(relationArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${function.name}${relation.name}Preservation", Seq(), Seq(judgment))
    // we now have the conclusion, we just need to choose the argument t_1 accordingly
    // find the input argument index of the with matching type
    val termIndex = function.inTypes.indexOf(function.successfulOutType)
    // use fresh or bound variables for all producer arguments,
    // *except* for t_1 which should be t_1
    val producerArgumentsConstraints = Constraint.freshOrBound(function.inTypes)
        .updated(termIndex, Constraint.Fixed(termVar))
    // use t_2 for the success variable
    val successVarPlacement = Constraint.Fixed(reducedTermVar)
    val refinements = selectSuccessfulApplication(baseLemma, function, producerArgumentsConstraints, successVarPlacement)
    refine(baseLemma, refinements)
  }

  /** Build a sequence of base predicate preservation lemmas for `predicate`. */
  def buildPredicatePreservationLemmas(predicate: FunctionDef): Seq[Lemma] = {
    // generate the conclusion first
    // --------------------
    // [predicate]([], ...)
    val outType = function.successfulOutType
    val predicateArgs = Assignments.generateSimpleSingle(predicate.inTypes)
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${function.name}${predicate.name}Preservation", Seq(), Seq(judgment))
    // now add a corresponding premise that invokes `function`:
    // [function]([], ...) =  []
    // function arguments can be fresh or bound with matching types
    val functionArgumentsConstraints = Constraint.freshOrBound(function.inTypes)
    // the success variable can be any of the arguments of ``predicate``, with matching types
    val matchingPredicateArgs = predicateArgs.filter(_.sortType == outType)
    val successVarConstraint = Constraint.Union(matchingPredicateArgs.map(Constraint.Fixed).toSet)
    // we do not need refinements that postulate that the function returns its argument
    val baseLemmas = refine(baseLemma,
      selectSuccessfulApplication(baseLemma, function, functionArgumentsConstraints, successVarConstraint)
      .filterNot(r => r.arguments contains FunctionMeta(r.result)))
    // add premises that postulate that `predicate` holds:
    // [predicate]([], ...)
    val evolvedLemmas = baseLemmas.flatMap(lemma => {
      val constraints = Constraint.freshOrBound(predicate.inTypes)
      refine(lemma, selectPredicate(lemma, predicate, constraints))
    })
    evolvedLemmas
  }

  /** Generate all base preservation lemmas. */
  override def generateBase(): Seq[Lemma] = {
    val lemmas = new mutable.MutableList[Lemma]()
    val outType = function.successfulOutType
    // if our successful return sort appears exactly once as an argument sort,
    // build relational preservation lemmas
    if(function.inTypes.count(_ == outType) == 1) {
      // find all matching preservable relations
      val relations = enquirer
        .retrievePreservables(Set(function.successfulOutType))
        .filter(_.inTypes == Seq(outType, outType))
      relations.foreach(relation => {
        lemmas ++= buildRelationalPreservationLemmas(relation)
      })
    }
    // build predicate preservation lemmas
    for(predicate <- enquirer.retrievePreservables(Set(outType)))
      if(predicate.inTypes.count(_ == outType) == 1)
       lemmas ++= buildPredicatePreservationLemmas(predicate)
    lemmas
  }

  override def expand(lemma: Lemma): Seq[Refinement] = {
    // build a map of predicates and producers of "in types"
    val predicates = enquirer.retrievePredicates(lemma.boundTypes)
    val producers = enquirer.retrieveProducers(lemma.boundTypes)
    // we just have to find matching premises
    val refinements = new mutable.MutableList[Refinement]()
    for(predicate <- predicates if predicate.isStatic)
      refinements ++= selectPredicate(lemma, predicate)
    for(fn <- producers if fn.isFailable && fn.isStatic && fn != function) {
      // allow to use bound success vars
      refinements ++= selectSuccessfulApplication(lemma, fn,
        Constraint.preferBound(fn.inTypes),
        Constraint.freshOrBound(fn.successfulOutType)
      )
    }
    refinements
  }
}
