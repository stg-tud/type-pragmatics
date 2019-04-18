package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.{Assignments, Constraint, StrategyHelpers}
import de.tu_darmstadt.veritas.backend.ast.FunctionExpJudgment
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionMeta}

import scala.collection.mutable

class PreservationStrategy(override val problem: Problem, function: FunctionDef)
  extends RefinementStrategy with StrategyHelpers {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Query._

  implicit private val enquirer: LemmaGenSpecEnquirer = problem.enquirer

  def buildRelationalPreservationLemmas(predicate: FunctionDef): Seq[Lemma] = {
    // build preservation lemma that postulates that ``predicate`` holds for
    // the original term and for the reduction step
    // -------------------------
    // [predicate]([t_1], [t_2])
    val predicateArgs = Assignments.generateSimpleSingle(predicate.inTypes)
    val Seq(termVar, reducedTermVar) = predicateArgs
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${function.name}${predicate.name}Preservation", Seq(), Seq(judgment))
    // we now have the conclusion, we just need to choose the input argument accordingly
    // find the input argument index of the ``producer``
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

  def buildPredicatePreservationLemmas(predicate: FunctionDef): Seq[Lemma] = {
    // --------------------
    // [predicate]([], ...)
    val outType = function.successfulOutType
    val predicateArgs = Assignments.generateSimpleSingle(predicate.inTypes)
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${function.name}${predicate.name}Preservation", Seq(), Seq(judgment))
    // [producer]([], ...) =  []
    // producer arguments can be fresh or bound with matching types
    val producerArgumentsConstraints = Constraint.freshOrBound(function.inTypes)
    // the success variable can be any of the arguments of ``predicate``, with matching types
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
    })
    /*baseLemmas ++*/ evolvedLemmas
  }

  override def generateBase(): Seq[Lemma] = {
    // find all argument positions which take successful output type
    // generate 2 types of preservation lemmas:
    val lemmas = new mutable.MutableList[Lemma]()
    val outType = function.successfulOutType
    if(function.inTypes.count(_ == outType) == 1) {
      val predicates = enquirer
        .retrievePreservables(Set(function.successfulOutType))
        .filter(_.inTypes == Seq(outType, outType))
      predicates.foreach(predicate => {
        lemmas ++= buildRelationalPreservationLemmas(predicate)
      })
    }
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
