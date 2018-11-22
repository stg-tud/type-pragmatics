package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import java.io.File

import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator

import scala.collection.mutable



class ShapedPool() {
  protected val pool: mutable.Map[LemmaShape, mutable.Set[Lemma]] =
    new mutable.HashMap[LemmaShape, mutable.Set[Lemma]]()

  def add(lemma: Lemma): Boolean = {
    val shape = lemma.shape()
    if(!pool.contains(shape))
      pool(shape) = new mutable.HashSet[Lemma]()
    val lemmaSet = pool(shape)
    if(!lemmaSet.contains(lemma)
      && !lemmaSet.exists(poolLemma => LemmaEquivalence.isEquivalent(poolLemma.rule, lemma.rule))) {
      lemmaSet += lemma
      true
    } else {
      false
    }
  }

  def lemmas: Seq[Lemma] = pool.valuesIterator.flatten.toSeq
}

class LemmaGenerator(specFile: File, maxPremises: Int) {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.queries.Query._

  type LemmaGeneration = mutable.MutableList[Lemma]

  private val spec = new ScalaSPLTranslator().translate(specFile)
  private val dsk = DomainSpecificKnowledgeBuilder().build(specFile)
  implicit private val enquirer = new LemmaGenSpecEnquirer(spec, dsk)

  private val pool = new ShapedPool()

  def constructAllChoices[T](choices: Seq[Seq[T]]): Seq[Seq[T]] = choices match {
    case currentChoices :: remainingChoices =>
      val constructedRemainingChoices: Seq[Seq[T]] = constructAllChoices(remainingChoices)
      for(currentChoice <- currentChoices; remainingChoice <- constructedRemainingChoices)
          yield currentChoice +: remainingChoice
    case Nil => Seq(Seq())
  }

  def buildSuccessLemma(function: FunctionDef): Lemma = {
    val (_, successConstructor) = enquirer.retrieveFailableConstructors(function.outType)
    val successName = FreshVariables.freshName(Map.empty, prefix = "success") // TODO: this is only to avoid name clashes
    val successVar = MetaVar(successName)
    successVar.typ = Some(TypeInference.Sort(successConstructor.in.head.name))
    val arguments = FreshVariables.freshMetaVars(Map.empty, function.inTypes)
    val invocationExp = FunctionExpApp(function.name, arguments.map {
      case (metaVar, typ) => FunctionMeta(metaVar)
    })
    val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
    val equality = enquirer.makeEquation(invocationExp, successExp).asInstanceOf[FunctionExpJudgment]
    val exists = ExistsJudgment(Seq(successVar), Seq(equality))
    new Lemma(
      arguments.toMap,
      TypingRule(s"${function.name}Progress", Seq(), Seq(exists)))
  }

  def buildPredicatePreservationLemmas(producer: FunctionDef, predicate: FunctionDef): Seq[Lemma] = {
    // build lemmas that postulate that ``predicate`` holds for the result of ``producer``
    // ``producer`` may be failable
    // might have multiple choices because ``predicate`` might have multiple arguments of compatible type
    val producerArgs = FreshVariables.freshMetaVars(Map.empty, producer.inTypes)
    val producerInvocation = FunctionExpApp(producer.name, producerArgs.map(v => FunctionMeta(v._1)))
    val productType = producer.successfulOutType
    val productHoles = predicate.inTypes.view.zipWithIndex.collect {
      case (typ, idx) if typ == productType => idx
    }
    var baseLemmas = Seq[Lemma]()
    for(hole <- productHoles) {
      val predicateArgs = FreshVariables.freshMetaVars(producerArgs.toMap, predicate.inTypes)
      val invocationExp = FunctionExpApp(predicate.name, predicateArgs.map {
        case (metaVar, typ) => FunctionMeta(metaVar)
      })
      val judgment = FunctionExpJudgment(invocationExp)
      val baseLemma = new Lemma(
        predicateArgs.toMap ++ producerArgs.toMap,
        TypingRule(s"${producer.name}${predicate.name}Preservation$hole", Seq(), Seq(judgment))
      )
      // we now have the conclusion, we just need to choose the input argument accordingly
      var right: FunctionExpMeta = FunctionMeta(predicateArgs(hole)._1)
      if(producer.isFailable) {
        val constructor = enquirer.retrieveFailableConstructors(producer.outType)._2
        right = FunctionExpApp(constructor.name, Seq(right))
      }
      val equality = enquirer.makeEquation(producerInvocation, right).asInstanceOf[FunctionExpJudgment]
      baseLemmas :+= baseLemma.withPremise(equality)
    }
    baseLemmas
  }

  def generateBasePreservationLemmas(dynamicFunctionName: String): Seq[Lemma] = {
    val dynamicFunction = dsk.dynamicFunctions.find(_.name == dynamicFunctionName).get
    // select all possible static predicates
    val predicates = enquirer.retrievePredicates(dynamicFunction.successfulOutType)
    val lemmas = mutable.HashSet.empty[Lemma]
    for(predicate <- predicates) {
      val baseLemmas = buildPredicatePreservationLemmas(dynamicFunction, predicate)
      lemmas ++= baseLemmas
      lemmas ++= baseLemmas.flatMap(selectPredicate(_, predicate))
    }
    lemmas.toSeq
  }

  def selectPredicate(baseLemma: Lemma, predicate: FunctionDef): Seq[Lemma] = {
    val (boundTypes, unboundTypes) = predicate.inTypes.partition(baseLemma.boundTypes.contains(_))
    val builder = new LemmaBuilder(baseLemma)
    builder.bindTypes(unboundTypes)
    // collect vars of suitable type for invocation
    val possibleArguments = predicate.inTypes.map(typ => builder.bindingsOfType(typ))
    val possibleChoices = constructAllChoices(possibleArguments)
    var lemmas = Seq[Lemma]()
    for(arguments <- possibleChoices) {
      val localBuilder = builder.copy()
      val invocationExp = FunctionExpJudgment(
        FunctionExpApp(
          predicate.name,
          arguments.map(v => FunctionMeta(v))
        )
      )
      if (!localBuilder.rule.premises.contains(invocationExp)) {
        localBuilder.addPremise(invocationExp)
        lemmas :+= localBuilder.build()
      }
    }
    lemmas
  }

  // TODO: This always selects a fresh variable for the success!
  def selectSuccessPredicate(baseLemma: Lemma, function: FunctionDef): Seq[Lemma] = {
    val builder = new LemmaBuilder(baseLemma)
    val (boundTypes, unboundTypes) = function.inTypes.partition(baseLemma.boundTypes.contains(_))
    builder.bindTypes(unboundTypes)
    // collect vars of suitable type for invocation
    val possibleArguments = function.inTypes.map(builder.bindingsOfType)
    val possibleChoices = constructAllChoices(possibleArguments)
    // bind success var *after* having constructed all choices for arguments, because
    // we don't want the success variable to appear as an argument here
    val (_, successConstructor) = enquirer.retrieveFailableConstructors(function.outType)
    val successVar = builder.bindType(successConstructor.in.head)
    var lemmas = Seq[Lemma]()
    for(arguments <- possibleChoices) {
      val localBuilder = builder.copy()
      val invocationExp = FunctionExpApp(
        function.name,
        arguments.map(v => FunctionMeta(v))
      )
      val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
      val equality = enquirer.makeEquation(invocationExp, successExp).asInstanceOf[FunctionExpJudgment]
      if (!localBuilder.rule.premises.contains(equality)) {
        val leftSides = localBuilder.rule.premises.collect {
          case (FunctionExpJudgment(FunctionExpEq(left, _))) => left
        }
        if(!leftSides.contains(invocationExp)) {
          localBuilder.addPremise(equality)
          lemmas :+= localBuilder.build()
        }
      }
    }
    lemmas
  }

  def generateProgressLemma(dynamicFunctionName: String): Lemma = {
    val dynamicFunction = dsk.dynamicFunctions.find(_.name == dynamicFunctionName).get
    // the function's return type must be failable
    buildSuccessLemma(dynamicFunction)
  }

  def evolveProgressLemma(generation: LemmaGeneration, lemma: Lemma) = {
    // build a map of predicates and producers of "in types"
    val predicates = lemma.boundTypes.flatMap(enquirer.retrievePredicates)
    val producers = lemma.boundTypes.flatMap(enquirer.retrieveProducers)
    val failableProducers = producers.filter(_.isFailable)
    val transformers = lemma.boundTypes.flatMap(enquirer.retrieveTransformers)
    val failableTransformers = transformers.filter(_.isFailable)
    // we just have to find matching premises
    // find predicates that involve any of the given types
    for(fn <- predicates)
      addChecked(generation, selectPredicate(lemma, fn))
    for(fn <- failableProducers if fn.isStatic)
      addChecked(generation, selectSuccessPredicate(lemma, fn))
    for(fn <- failableTransformers if fn.isStatic)
      addChecked(generation, selectSuccessPredicate(lemma, fn))
  }

  def addChecked(generation: LemmaGeneration, lemmas: Seq[Lemma]): Unit = {
    lemmas
      .withFilter(_.rule.premises.length <= maxPremises)
      .foreach(lemma => {
        if(pool.add(lemma))
          generation += lemma
      })
  }

  def generateProgressLemmas(dynamicFunctionName: String): Seq[Lemma] = {
    var generation = new mutable.MutableList[Lemma]()
    generation += generateProgressLemma(dynamicFunctionName)
    while(generation.nonEmpty) {
      val nextGeneration = new mutable.MutableList[Lemma]()
      generation.foreach(evolveProgressLemma(nextGeneration, _))
      generation = nextGeneration
    }
    pool.lemmas
  }

  def evolvePreservationLemma(generation: LemmaGeneration, lemma: Lemma): Unit = {
    // STATIC(exp) (+ DYNAMIC(exp')) => STATIC(exp')
    val predicates = lemma.boundTypes.flatMap(enquirer.retrievePredicates)
    val producers = lemma.boundTypes.flatMap(enquirer.retrieveProducers)
    val failableProducers = producers.filter(_.isFailable)
    val transformers = lemma.boundTypes.flatMap(enquirer.retrieveTransformers)
    val failableTransformers = transformers.filter(_.isFailable)
    // we just have to find matching premises
    // find predicates that involve any of the given types
    for(fn <- predicates)
      addChecked(generation, selectPredicate(lemma, fn))
    for(fn <- failableProducers)
      addChecked(generation, selectSuccessPredicate(lemma, fn))
    for(fn <- failableTransformers)
      addChecked(generation, selectSuccessPredicate(lemma, fn))
  }

  def generatePreservationLemmas(dynamicFunctionName: String): Seq[Lemma] = {
    var generation = new mutable.MutableList[Lemma]()
    generation ++= generateBasePreservationLemmas(dynamicFunctionName)
    while(generation.nonEmpty) {
      val nextGeneration = new mutable.MutableList[Lemma]()
      generation.foreach(evolvePreservationLemma(nextGeneration, _))
      generation = nextGeneration
    }
    pool.lemmas
  }
}
