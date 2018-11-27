package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, MetaVar}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.backend.util.FreeVariables

import scala.collection.mutable

class PreservationStrategy(override val problem: Problem, producer: FunctionDef)
  extends RefinementStrategy with StrategyHelpers {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.queries.Query._

  implicit private val enquirer: LemmaGenSpecEnquirer = problem.enquirer

  /*def buildPredicatePreservationLemmas(predicate: FunctionDef): Seq[Lemma] = {
    // build lemmas that postulate that ``predicate`` holds for the result of ``producer``
    // ``producer`` may be failable
    // might have multiple choices because ``predicate`` might have multiple arguments of compatible type
    val producerArgs = FreshVariables.freshMetaVars(Set(), producer.inTypes)
    val producerInvocation = FunctionExpApp(producer.name, Assignments.wrapMetaVars(producerArgs))
    val productType = producer.successfulOutType
    val productHoles = predicate.inTypes.view.zipWithIndex.collect {
      case (typ, idx) if typ == productType => idx
    }
    var baseLemmas = Seq[Lemma]()
    for(hole <- productHoles) {
      val predicateArgs = FreshVariables.freshMetaVars(producerArgs.toSet, predicate.inTypes)
      val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
      val judgment = FunctionExpJudgment(invocationExp)
      val baseLemma = new Lemma(s"${producer.name}${predicate.name}Preservation$hole", Seq(), Seq(judgment))
      // we now have the conclusion, we just need to choose the input argument accordingly
      var right: FunctionExpMeta = FunctionMeta(predicateArgs(hole))
      if(producer.isFailable) {
        val (_, constructor) = enquirer.retrieveFailableConstructors(producer.outType)
        right = FunctionExpApp(constructor.name, Seq(right))
      }
      val equality = enquirer.makeEquation(producerInvocation, right).asInstanceOf[FunctionExpJudgment]
      baseLemmas :+= baseLemma.addPremise(equality)
    }
    baseLemmas
  }*/

  def buildReductionPredicateLemmas(predicate: FunctionDef): Seq[Lemma] = {
    // build lemmas that postulate that ``predicate`` holds for the result of ``producer``
    // ``producer`` may be failable
    // might have multiple choices because ``predicate`` might have multiple arguments of compatible type
    val predicateArgs = FreshVariables.freshMetaVars(Set(), predicate.inTypes)
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${producer.name}${predicate.name}Preservation", Seq(), Seq(judgment))
    // we now have the conclusion, we just need to choose the input argument accordingly
    /*var right: FunctionExpMeta = FunctionMeta(predicateArgs(hole))
    if(producer.isFailable) {
      val (_, constructor) = enquirer.retrieveFailableConstructors(producer.outType)
      right = FunctionExpApp(constructor.name, Seq(right))
    }
    //val equality = enquirer.makeEquation(producerInvocation, right).asInstanceOf[FunctionExpJudgment]
    */
    val termIndex = producer.inTypes.indexOf(producer.successfulOutType)
    val producerArgumentsPlacements =
      Assignments.freshOrBoundPlacements(producer.inTypes).updated(termIndex, Assignments.Fixed(predicateArgs(0)))
    val successVarPlacement = Assignments.Fixed(predicateArgs(1))
    refine(baseLemma, selectSuccessPredicate(baseLemma, producer, producerArgumentsPlacements, successVarPlacement, Set()))
  }

  def buildPredicatePreservationLemmas(predicate: FunctionDef): Seq[Lemma] = {
    val producerArgs = FreshVariables.freshMetaVars(Set(), producer.inTypes)
    val predicateArgs = FreshVariables.freshMetaVars(producerArgs.toSet, predicate.inTypes)
    val invocationExp = FunctionExpApp(predicate.name, Assignments.wrapMetaVars(predicateArgs))
    val judgment = FunctionExpJudgment(invocationExp)
    val baseLemma = new Lemma(s"${producer.name}${predicate.name}Preservation", Seq(), Seq(judgment))

    /*val productHoles = predicate.inTypes.view.zipWithIndex.collect {
      case (typ, idx) if typ == productType => idx
    }
    var baseLemmas = Seq[Lemma]()
    for(hole <- productHoles) {

      val baseLemma = new Lemma(s"${producer.name}${predicate.name}Preservation$hole", Seq(), Seq(judgment))
      // we now have the conclusion, we just need to choose the input argument accordingly
      var right: FunctionExpMeta = FunctionMeta(predicateArgs(hole))
      if(producer.isFailable) {
        val (_, constructor) = enquirer.retrieveFailableConstructors(producer.outType)
        right = FunctionExpApp(constructor.name, Seq(right))
      }
      val equality = enquirer.makeEquation(producerInvocation, right).asInstanceOf[FunctionExpJudgment]
      baseLemmas :+= baseLemma.addPremise(equality)

    }*/
    val producerArgumentsPlacements = Assignments.freshOrBoundPlacements(producer.inTypes)
    var matchingPredicateArgs = predicateArgs.filter(_.sortType == producer.successfulOutType)
    val successVarPlacement = Assignments.Union(matchingPredicateArgs.map(Assignments.Fixed(_)).toSet)
    val baseLemmas = refine(baseLemma, selectSuccessPredicate(baseLemma, producer, producerArgumentsPlacements, successVarPlacement,
      matchingPredicateArgs.toSet
    ))
    baseLemmas.flatMap(lemma =>
      refine(lemma, selectPredicate(lemma, predicate))
    )
  }

  override def generateBase(): Seq[Lemma] = {
    // find all argument positions which take successful output type
    // generate 2 types of preservation lemmas:
    val lemmas = new mutable.MutableList[Lemma]()
    if(producer.inTypes.count(_ == producer.successfulOutType) == 1) {
      val predicates = enquirer
        .retrievePredicates(producer.successfulOutType)
        .filter(_.inTypes.length == 2)
        .filter(predicate => predicate.inTypes.count(_ == producer.successfulOutType) == 2)
      predicates.foreach(predicate => {
        lemmas ++= buildReductionPredicateLemmas(predicate)
      })
    }
    val predicates = enquirer.retrievePredicates(producer.successfulOutType)
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
      // allow to use success vars of matching type, but only if they are used in the consequences!
      val additionalSuccessVars = lemma
        .bindingsOfType(fn.successfulOutType)
        .intersect(FreeVariables.freeVariables(lemma.consequences))
      refinements ++= selectSuccessPredicate(lemma, fn, additionalSuccessVars = additionalSuccessVars)
    }
    for(fn <- transformers if fn.isFailable && fn.isStatic && fn != producer)
      refinements ++= selectSuccessPredicate(lemma, fn)
    refinements
  }
}
