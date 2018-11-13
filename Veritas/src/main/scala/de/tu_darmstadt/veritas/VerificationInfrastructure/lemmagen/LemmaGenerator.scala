package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import java.io.File

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator

import scala.collection.mutable

class LemmaGenerator(specFile: File) {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.queries.Query._

  private val spec = new ScalaSPLTranslator().translate(specFile)
  private val dsk = DomainSpecificKnowledgeBuilder().build(specFile)
  implicit private val enquirer = new LemmaGenSpecEnquirer(spec, dsk)

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
    // might have multiple choices because ``predicate`` might have multiple arguments of compatible type
    val predicateArgs = FreshVariables.freshMetaVars(Map.empty, predicate.inTypes)
    val invocationExp = FunctionExpApp(predicate.name, predicateArgs.map {
      case (metaVar, typ) => FunctionMeta(metaVar)
    })
    val judgment = FunctionExpJudgment(invocationExp)
    // we now have the conclusion, we just need to choose the input argument accordingly
    val baseLemma = new Lemma(
      predicateArgs.toMap,
      TypingRule(s"${producer.name}${predicate.name}Preservation", Seq(), Seq(judgment))
    )
    val builder = new LemmaBuilder(baseLemma)
    builder.bindTypes(producer.inTypes)
    // could be empty if the predicate and producer do not share any argument types
    val possibleArguments = producer.inTypes.map(builder.bindingsOfType)
    val possibleChoices = constructAllChoices(possibleArguments)
    var lemmas = Seq[Lemma]()
    for(mv <- builder.bindingsOfType(producer.outType)) {
      for (arguments <- possibleChoices) {
        val localBuilder = builder.copy()
        val left = FunctionMeta(mv)
        val right = FunctionExpApp(producer.name, arguments.map(v => FunctionMeta(v)))
        val premise = enquirer.makeEquation(left, right).asInstanceOf[FunctionExpJudgment]
        localBuilder.addPremise(premise)
        lemmas :+= localBuilder.build()
      }
    }
    lemmas
  }

  def generatePreservationLemmas(dynamicFunctionName: String): Seq[Lemma] = {
    val dynamicFunction = dsk.dynamicFunctions.find(_.name == dynamicFunctionName).get
    // select all possible static predicates
    val predicates = enquirer.retrievePredicates(dynamicFunction.outType)
    predicates.flatMap(buildPredicatePreservationLemmas(dynamicFunction, _)).toSeq
  }

  def selectPredicate(baseLemma: Lemma, predicate: FunctionDef): Seq[Lemma] = {
    val (boundTypes, unboundTypes) = predicate.inTypes.partition(baseLemma.boundTypes.contains(_))
    val builder = new LemmaBuilder(baseLemma)
    builder.bindTypes(unboundTypes)
    // collect vars of suitable type for invocation
    val possibleArguments = predicate.inTypes.map(builder.bindingsOfType)
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
        localBuilder.addPremise(equality)
        lemmas :+= localBuilder.build()
      }
    }
    lemmas
  }

  def generateProgressLemma(dynamicFunctionName: String): Lemma = {
    val dynamicFunction = dsk.dynamicFunctions.find(_.name == dynamicFunctionName).get
    // the function's return type must be failable
    buildSuccessLemma(dynamicFunction)
  }

  def evolveProgressLemma(lemma: Lemma): Iterable[Lemma] = {
    // build a map of predicates and producers of "in types"
    val predicates = lemma.boundTypes.flatMap(enquirer.retrievePredicates)
    val producers = lemma.boundTypes.flatMap(enquirer.retrieveProducers)
    val failableProducers = producers.filter(_.isFailable)
    val transformers = lemma.boundTypes.flatMap(enquirer.retrieveTransformers)
    val failableTransformers = transformers.filter(_.isFailable)
    // we just have to find matching premises
    // find predicates that involve any of the given types
    val lemmas = mutable.HashSet.empty[Lemma]
    for(fn <- predicates)
      lemmas ++= selectPredicate(lemma, fn)
    for(fn <- failableProducers if fn.isStatic)
      lemmas ++= selectSuccessPredicate(lemma, fn)
    for(fn <- failableTransformers if fn.isStatic)
      lemmas ++= selectSuccessPredicate(lemma, fn)
    lemmas
  }

  def evolveProgressLemmas(lemmas: Stream[Lemma]): Stream[Lemma] = {
    val evolved = lemmas.flatMap(evolveProgressLemma)
    evolved #::: evolveProgressLemmas(evolved)
  }

  def generateProgressLemmas(dynamicFunctionName: String): Stream[Lemma] = {
    val base = generateProgressLemma(dynamicFunctionName)
    base #:: evolveProgressLemmas(Stream(base))
  }

  def evolvePreservationLemma(lemma: Lemma): Iterable[Lemma] = {
    // STATIC(exp) (+ DYNAMIC(exp')) => STATIC(exp')
    // get staitic
    Seq()
  }
}
