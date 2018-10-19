package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import java.io.File

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference.Sort
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator

import scala.collection.mutable

class LemmaGenerator(specFile: File) {
  private val spec = new ScalaSPLTranslator().translate(specFile)
  private val dsk = DomainSpecificKnowledgeBuilder().build(specFile)
  private val enquirer = new LemmaGenSpecEnquirer(spec, dsk)

  private var ctr = 0

  def generateMetaVar(typ: SortRef): MetaVar = {
    val meta = MetaVar(s"x$ctr")
    ctr += 1
    meta.typ = Some(Sort(typ.name))
    meta
  }

  def evolveProgressLemma(lemma: Lemma): Unit = {

  }

  def buildSuccessLemma(function: FunctionDef): Lemma = {
    val arguments = function.signature.in.map(ref => (generateMetaVar(ref), ref))
    val (_, successConstructor) = enquirer.retrieveFailableConstructors(function.signature.out)
    val successVar = generateMetaVar(function.signature.out)
    val invocationExp = FunctionExpApp(function.signature.name, arguments.map {
      case (name, typ) => FunctionMeta(name)
    })
    val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
    val equality = enquirer.makeEquation(invocationExp, successExp).asInstanceOf[FunctionExpJudgment]
    val exists = ExistsJudgment(Seq(successVar), Seq(equality))
    new Lemma(arguments.toMap, TypingRule(s"${function.signature.name}Progress", Seq(), Seq(exists)))
  }

  def selectPredicate(baseLemma: Lemma, predicate: FunctionDef): Lemma = {
    val (boundTypes, unboundTypes) = predicate.signature.in.partition(baseLemma.boundTypes.contains(_))
    val newMetaVars = unboundTypes.map(generateMetaVar)
    var lemma = baseLemma.bind(newMetaVars:_*)
    // collect vars of suitable type for invocation TODO: there might be choices here
    val arguments = predicate.signature.in.map(typ => lemma.bindings.find(_._2 == typ).get._1)
    val invocationExp = FunctionExpJudgment(
      FunctionExpApp(
        predicate.signature.name,
        arguments.map(v => FunctionMeta(v))
      )
    )
    lemma.withPremise(invocationExp)
  }

  def generateProgressLemma(dynamicFunctionName: String): Lemma = {
    val dynamicFunction = dsk.dynamicFunctions.find(_.signature.name == dynamicFunctionName).get
    // the function's return type must be failable
    var lemma = buildSuccessLemma(dynamicFunction)
    // build a map of predicates and producers of "in types"
    val predicates = lemma.boundTypes.flatMap(enquirer.retrievePredicates)
    val producers = lemma.boundTypes.flatMap(enquirer.retrieveProducers)
    val failableProducers = producers.filter(defn => enquirer.isFailableType(defn.signature.out))
    // we just have to find matching premises
    // find predicates that involve any of the given types
    println(predicates.map(_.signature.name))
    println(failableProducers.map(_.signature.name))
    selectPredicate(lemma, predicates.head)
  }
}
