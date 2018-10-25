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

  def selectPredicate(baseLemma: Lemma, predicate: FunctionDef): Lemma = {
    val (boundTypes, unboundTypes) = predicate.inTypes.partition(baseLemma.boundTypes.contains(_))
    val builder = new LemmaBuilder(baseLemma)
    builder.bindTypes(unboundTypes)
    // collect vars of suitable type for invocation TODO: there might be choices here
    val arguments = predicate.inTypes.map(typ => builder.bindings.find(_._2 == typ).get._1)
    val invocationExp = FunctionExpJudgment(
      FunctionExpApp(
        predicate.name,
        arguments.map(v => FunctionMeta(v))
      )
    )
    if(!builder.rule.premises.contains(invocationExp))
      builder.addPremise(invocationExp)
    builder.build()
  }

  def selectSuccessPredicate(baseLemma: Lemma, function: FunctionDef): Lemma = {
    val builder = new LemmaBuilder(baseLemma)
    val (_, successConstructor) = enquirer.retrieveFailableConstructors(function.outType)
    val successVar = builder.bindType(successConstructor.in.head)
    val (boundTypes, unboundTypes) = function.inTypes.partition(baseLemma.boundTypes.contains(_))
    builder.bindTypes(unboundTypes)
    // collect vars of suitable type for invocation TODO: there might be choices here
    val arguments = function.inTypes.map(typ => builder.bindings.find(_._2 == typ).get._1)
    val invocationExp = FunctionExpApp(
      function.name,
      arguments.map(v => FunctionMeta(v))
    )
    val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
    val equality = enquirer.makeEquation(invocationExp, successExp).asInstanceOf[FunctionExpJudgment]
    if(!builder.rule.premises.contains(equality))
      builder.addPremise(equality)
    builder.build()
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
      lemmas += selectPredicate(lemma, fn)
    for(fn <- failableProducers if fn.isStatic)
      lemmas += selectSuccessPredicate(lemma, fn)
    for(fn <- failableTransformers if fn.isStatic)
      lemmas += selectSuccessPredicate(lemma, fn)
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
}
