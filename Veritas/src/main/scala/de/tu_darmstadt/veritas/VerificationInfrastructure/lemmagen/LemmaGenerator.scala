package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import java.io.File

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionExpApp, FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator

import scala.collection.mutable

class LemmaGenerator(specFile: File) {
  private val spec = new ScalaSPLTranslator().translate(specFile)
  private val dsk = DomainSpecificKnowledgeBuilder().build(specFile)
  private val enquirer = new LemmaGenSpecEnquirer(spec, dsk)

  private var ctr = 0

  def generateMetaVar(): MetaVar = {
    ctr += 1
    MetaVar(s"x$ctr")
  }

  def evolveProgressLemma(lemma: Lemma): Unit = {

  }

  def buildSuccessLemma(function: FunctionDef): Lemma = {
    val arguments = function.signature.in.map(ref => (generateMetaVar(), ref))
    val (_, successConstructor) = enquirer.retrieveFailableConstructors(function.signature.out)
    val successVar = generateMetaVar()
    val invocationExp = FunctionExpApp(function.signature.name, arguments.map {
      case (name, typ) => FunctionMeta(name)
    })
    val successExp = FunctionExpApp(successConstructor.name, Seq(FunctionMeta(successVar)))
    val equality = enquirer.makeEquation(invocationExp, successExp).asInstanceOf[FunctionExpJudgment]
    val exists = ExistsJudgment(Seq(successVar), Seq(equality))
    new Lemma(arguments.toMap, TypingRule(s"${function.signature.name}Progress", Seq(), Seq(exists)))
  }

  def generateProgressLemma(dynamicFunctionName: String): Lemma = {
    val dynamicFunction = dsk.dynamicFunctions.find(_.signature.name == dynamicFunctionName).get
    // the function's return type must be failable
    var lemma = buildSuccessLemma(dynamicFunction)
lemma
    // build a map of predicates and producers of "in types"
    /*val predicates = Map(inTypes.map(ref => (ref, enquirer.retrievePredicates(ref))):_*)
    val producers = Map(inTypes.map(ref => (ref, enquirer.retrieveProducers(ref))):_*)
    val failableProducers = producers.map {
      case (ref, defs) => (ref, defs.filter(defn => enquirer.isFailableType(defn.signature.out)))
    }.toMap




    // we just have to find matching premises
    // find predicates that involve any of the given types
    val matchingPredicates = getBoundTypes().flatMap(predicates(_))
    val matchingFailableProducers = getBoundTypes().flatMap(failableProducers(_))
    println(matchingPredicates)
    println(matchingFailableProducers)*/
  }
}
