package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.assignments.{Assignments, Constraint}
import de.tu_darmstadt.veritas.backend.ast.FunctionExpJudgment
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionMeta}
import de.tu_darmstadt.veritas.backend.util.FreeVariables

import scala.collection.mutable

class PreservationStrategy(override val problem: Problem, producer: FunctionDef)
  extends RefinementStrategy with StrategyHelpers {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.queries.Query._

  implicit private val enquirer: LemmaGenSpecEnquirer = problem.enquirer

  def buildReductionPredicateLemmas(predicate: FunctionDef): Seq[Lemma] = {
    // build preservation lemma that postules that ``predicate`` holds for
    // the original term and for the reduction step
    val predicateArgs = Assignments.generateSimpleSingle(predicate.inTypes)
    val Seq(termVar, reducedTermVar) = predicateArgs
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${producer.name}${predicate.name}Preservation", Seq(), Seq(judgment))
    // we now have the conclusion, we just need to choose the input argument accordingly
    val termIndex = producer.inTypes.indexOf(producer.successfulOutType)
    val producerArgumentsConstraints = Constraint.freshOrBound(producer.inTypes)
        .updated(termIndex, Constraint.Fixed(termVar))
    val successVarPlacement = Constraint.Fixed(reducedTermVar)
    val refinements = selectSuccessPredicate(baseLemma, producer, producerArgumentsConstraints, successVarPlacement)
    refine(baseLemma, refinements)
  }

  def buildPredicatePreservationLemmas(predicate: FunctionDef): Seq[Lemma] = {
    val predicateArgs = FreshVariables.freshMetaVars(Set(), predicate.inTypes)
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${producer.name}${predicate.name}Preservation", Seq(), Seq(judgment))
    val producerArgumentsConstraints = Constraint.freshOrBound(producer.inTypes)
    var matchingPredicateArgs = predicateArgs.filter(_.sortType == producer.successfulOutType)
    val successVarPlacement = Constraint.Union(matchingPredicateArgs.map(Constraint.Fixed(_)).toSet)
    val baseLemmas = refine(baseLemma, selectSuccessPredicate(baseLemma, producer, producerArgumentsConstraints, successVarPlacement)
      .filterNot(r => r.arguments contains FunctionMeta(r.result)))
    val l = baseLemmas.flatMap(lemma => {
      val a = predicateArgs.map {
        case mv if mv.sortType == producer.successfulOutType => Constraint.Exclude(
          Constraint.Union(Set(Constraint.Bound(producer.successfulOutType), Constraint.Fresh(producer.successfulOutType))),
          Constraint.Fixed(mv))
        case x => Constraint.Union(Set(Constraint.Bound(x.sortType), Constraint.Fresh(x.sortType)))
      }
      refine(lemma, selectPredicate(lemma, predicate, a))
    })
    l ++ baseLemmas
  }

  override def generateBase(): Seq[Lemma] = {
    // find all argument positions which take successful output type
    // generate 2 types of preservation lemmas:
    val lemmas = new mutable.MutableList[Lemma]()
    val outType = producer.successfulOutType
    if(producer.inTypes.count(_ == outType) == 1) {
      val predicates = enquirer
        .retrievePredicates(producer.successfulOutType)
        .filter(_.inTypes == Seq(outType, outType))
      predicates.foreach(predicate => {
        lemmas ++= buildReductionPredicateLemmas(predicate)
      })
    }
    val predicates = enquirer.retrievePredicates(outType)
    predicates.foreach({
      lemmas ++= buildPredicatePreservationLemmas(_)
    })
    lemmas
  }

  override def expand(lemma: Lemma): Seq[Refinement] = {
    // build a map of predicates and producers of "in types"
    val predicates = lemma.boundTypes.flatMap(enquirer.retrievePredicates)
    val producers = lemma.boundTypes.flatMap(enquirer.retrieveProducers)
    val transformers = lemma.boundTypes.flatMap(enquirer.retrieveTransformers)
    // we just have to find matching premises
    val refinements = new mutable.MutableList[Refinement]()
    for(predicate <- predicates if predicate.isStatic)
      refinements ++= selectPredicate(lemma, predicate)
    for(fn <- producers if fn.isFailable && fn.isStatic && fn != producer) {
      // allow to use bound success vars
      refinements ++= selectSuccessPredicate(lemma, fn,
        Constraint.preferBound(fn.inTypes),
        Constraint.freshOrBound(fn.successfulOutType)
      )
    }
    for(fn <- transformers if fn.isFailable && fn.isStatic && fn != producer)
      refinements ++= selectSuccessPredicate(lemma, fn)
    refinements
  }
}
