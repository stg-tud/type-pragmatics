package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.FunctionExpJudgment
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionExpMeta, FunctionMeta}

import scala.collection.mutable

class PreservationStrategy(override val problem: Problem, producer: FunctionDef)
  extends RefinementStrategy with StrategyHelpers {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.queries.Query._

  implicit private val enquirer = problem.enquirer

  def buildPredicatePreservationLemmas(predicate: FunctionDef): Seq[Lemma] = {
    // build lemmas that postulate that ``predicate`` holds for the result of ``producer``
    // ``producer`` may be failable
    // might have multiple choices because ``predicate`` might have multiple arguments of compatible type
    val producerArgs = FreshVariables.freshMetaVars(Set(), producer.inTypes)
    val producerInvocation = FunctionExpApp(producer.name, producerArgs.map(v => FunctionMeta(v._1)))
    val productType = producer.successfulOutType
    val productHoles = predicate.inTypes.view.zipWithIndex.collect {
      case (typ, idx) if typ == productType => idx
    }
    var baseLemmas = Seq[Lemma]()
    for(hole <- productHoles) {
      val predicateArgs = FreshVariables.freshMetaVars(producerArgs.map(_._1).toSet, predicate.inTypes)
      val invocationExp = FunctionExpApp(predicate.name, predicateArgs.map {
        case (metaVar, typ) => FunctionMeta(metaVar)
      })
      val judgment = FunctionExpJudgment(invocationExp)
      val baseLemma = new Lemma(s"${producer.name}${predicate.name}Preservation$hole", Seq(), Seq(judgment))
      // we now have the conclusion, we just need to choose the input argument accordingly
      var right: FunctionExpMeta = FunctionMeta(predicateArgs(hole)._1)
      if(producer.isFailable) {
        val constructor = enquirer.retrieveFailableConstructors(producer.outType)._2
        right = FunctionExpApp(constructor.name, Seq(right))
      }
      val equality = enquirer.makeEquation(producerInvocation, right).asInstanceOf[FunctionExpJudgment]
      baseLemmas :+= baseLemma.addPremise(equality)
    }
    baseLemmas
  }

  override def generateBase(): Seq[Lemma] = {
    // select all possible static predicates
    val predicates = enquirer.retrievePredicates(producer.successfulOutType)
    val lemmas = new mutable.MutableList[Lemma]()
    for(predicate <- predicates) {
      val baseLemmas = buildPredicatePreservationLemmas(predicate)
      lemmas ++= baseLemmas
      lemmas ++= baseLemmas.flatMap(lemma => {
        val refinements = selectPredicate(lemma, predicate)
        refinements.map(_.refine(problem, lemma))
      })
    }
    lemmas
  }

  override def expand(lemma: Lemma): Seq[Refinement] = {
    // build a map of predicates and producers of "in types"
    val predicates = lemma.boundTypes.flatMap(enquirer.retrievePredicates)
    val producers = lemma.boundTypes.flatMap(enquirer.retrieveProducers)
    val failableProducers = producers.filter(_.isFailable)
    val transformers = lemma.boundTypes.flatMap(enquirer.retrieveTransformers)
    val failableTransformers = transformers.filter(_.isFailable)
    // we just have to find matching premises
    val refinements = new mutable.MutableList[Refinement]()
    for(predicate <- predicates)
      refinements ++= selectPredicate(lemma, predicate)
    for(fn <- failableProducers)
      refinements ++= selectSuccessPredicate(lemma, fn)
    for(fn <- failableTransformers)
      refinements ++= selectSuccessPredicate(lemma, fn)
    refinements
  }
}
