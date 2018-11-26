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

  def buildPredicatePreservationLemmas(predicate: FunctionDef): Lemma = {
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
    baseLemma
  }

  override def generateBase(): Seq[Lemma] = {
    // find all argument positions which take successful output type
    val inputReductionIndices = producer.filterArguments(_ == producer.successfulOutType)
    // generate 2 types of preservation lemmas:
    val lemmas = new mutable.MutableList[Lemma]()
    if(inputReductionIndices.nonEmpty) {
      // (1) find predicates which take |inputReductionIndices| + 1 arguments of type successfuLOutType
      val predicates = enquirer
        .retrievePredicates(producer.successfulOutType)
        .filter(predicate => predicate.inTypes.count(_ == producer.successfulOutType) == inputReductionIndices.length + 1)
      val baseLemmas = predicates.map(buildPredicatePreservationLemmas)
      lemmas ++= baseLemmas.flatMap(lemma => {
        val refinements = selectSuccessPredicate(lemma, producer,
          freshSuccessVar = false,
          additionalSuccessVars = lemma.bindingsOfType(producer.successfulOutType))
          .filterNot(r => r.arguments contains r.result)
        refine(lemma, refinements)
      })
    }
    //val predicates = enquirer.retrievePredicates(producer.successfulOutType)
    /*
    for(predicate <- predicates if predicate.isStatic) {
      val baseLemmas = buildPredicatePreservationLemmas(predicate)
      //lemmas ++= baseLemmas
      // we just assume that the exact same predicate appears in the premises again for other values
      lemmas ++= baseLemmas.flatMap(lemma =>
        refine(lemma, selectPredicate(lemma, predicate))
      )
    }*/
    // select all
    // in many cases, failable producers for all other arguments of the consequent predicate appear in the premises
    // TODO: this does not help for projectCols because we select
    // projectTypeAttrL(al, tt) == someTType(tt)
    /*for(lemma <- lemmas.clone()) {
      lemma.consequences.head match {
        case FunctionExpJudgment(FunctionExpApp(_, args)) =>
          val otherArguments = args.collect {
            case FunctionMeta(mv) if mv.sortType != producer.successfulOutType => mv
          }
          // find failable producers for these arguments
          val allFailableProducers = otherArguments.collect {
            case mv => (mv, enquirer.retrieveProducers(mv.sortType).filter(_.isFailable))
          }
          // select them
          for((mv, failableProducers) <- allFailableProducers;
              failableProducer <- failableProducers) {
            lemmas ++= refine(lemma, selectSuccessPredicate(lemma, failableProducer,
              freshSuccessVar = false, additionalSuccessVars = Set(mv)))
            println()
          }
        case _ =>
      }
    }*/
    lemmas
  }

  override def expand(lemma: Lemma): Seq[Refinement] = {
    // build a map of predicates and producers of "in types"
    val predicates = lemma.boundTypes.flatMap(enquirer.retrievePredicates)
    val producers = lemma.boundTypes.flatMap(enquirer.retrieveProducers)
    val transformers = lemma.boundTypes.flatMap(enquirer.retrieveTransformers)
    // we just have to find matching premises
    val refinements = new mutable.MutableList[Refinement]()
    for(predicate <- predicates)
      refinements ++= selectPredicate(lemma, predicate)
    for(fn <- producers if fn.isFailable) {
      // allow to use success vars of matching type, but only if they are used in the consequences!
      val additionalSuccessVars = lemma
        .bindingsOfType(fn.successfulOutType)
        .intersect(FreeVariables.freeVariables(lemma.consequences))
      refinements ++= selectSuccessPredicate(lemma, fn, additionalSuccessVars = additionalSuccessVars)
    }
    for(fn <- transformers if fn.isFailable)
      refinements ++= selectSuccessPredicate(lemma, fn)
    refinements
  }
}
