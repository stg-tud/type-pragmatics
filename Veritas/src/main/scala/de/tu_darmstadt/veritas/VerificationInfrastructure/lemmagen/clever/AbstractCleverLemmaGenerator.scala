package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction.{GraphConstructor, PredicatePreservationConstructor, ProgressConstructor, RelationalPreservationConstructor}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaGenerator, Problem}
import de.tu_darmstadt.veritas.backend.ast.SortRef
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

import scala.collection.mutable

abstract class AbstractCleverLemmaGenerator(problem: Problem) extends LemmaGenerator {
  def makePipeline(constructor: GraphConstructor): LemmaGeneratorPipeline

  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.Query._
  implicit private val enquirer = problem.enquirer

  def isRelation(fn: FunctionDef): Boolean = {
    fn.inTypes.length == 2 && fn.inTypes.head == fn.inTypes(1)
  }

  def getPreservablesInvolving(termType: SortRef): Set[FunctionDef] = {
    enquirer.retrievePredicates(Set(termType)) intersect problem.dsk.preservables
  }

  def getPredicatesInvolving(termType: SortRef): Set[FunctionDef] = {
    getPreservablesInvolving(termType).filterNot(isRelation)
  }

  def getRelationsInvolving(termType: SortRef): Set[FunctionDef] = {
    getPreservablesInvolving(termType).filter(isRelation)
  }

  def preservationFunctions: Set[FunctionDef] = {
    problem.enquirer.dynamicFunctions.filter(fn =>
      fn.outType.name != "Bool" && fn.inTypes.contains(fn.successfulOutType)
    )
  }

  def progressFunctions: Set[FunctionDef] = {
    problem.enquirer.dynamicFunctions.filter(fn => problem.enquirer.isFailableType(fn.outType))
  }

  def generatePreservationLemmas(fn: FunctionDef): Seq[Lemma] = {
    val result = new mutable.HashSet[Lemma]()
    for(predicate <- getPredicatesInvolving(fn.successfulOutType)) { // TODO: make this exactly one argument of sort?
      result ++= tryInvokePipeline(makePredicatePreservationGraphConstructor(fn, predicate))
    }
    if(fn.inTypes.count(_ == fn.successfulOutType) >= 1) {
      for (relation <- getRelationsInvolving(fn.successfulOutType)) {
        val matchingIndices = fn.inTypes.zipWithIndex.collect {
          case (inType, idx) if inType == fn.successfulOutType => idx
        }
        for(idx <- matchingIndices) {
          result ++= tryInvokePipeline(makeRelationalPreservationGraphConstructor(fn, relation, idx))
        }
      }
    }
    result.toSeq
  }

  def tryInvokePipeline(constructor: Option[GraphConstructor]): Seq[Lemma] = {
    constructor.map(makePipeline(_).invokePipeline()).getOrElse(Seq())
  }

  override def generatePreservationLemmas(): Map[FunctionDef, Seq[Lemma]] = {
    preservationFunctions.map(fn => fn -> generatePreservationLemmas(fn)).toMap
  }

  def generateProgressLemmas(fn: FunctionDef): Seq[Lemma] = {
    tryInvokePipeline(makeProgressGraphConstructor(fn))
  }

  override def generateProgressLemmas(): Map[FunctionDef, Seq[Lemma]] = {
    progressFunctions.map(fn => fn -> generateProgressLemmas(fn)).toMap
  }

  def generate(): Map[FunctionDef, Seq[Lemma]] = {
    val lemmas = new mutable.HashMap[FunctionDef, Seq[Lemma]]().withDefaultValue(Seq())
    for((fn, progressLemmas) <- generateProgressLemmas())
      lemmas(fn) = progressLemmas
    for((fn, preservationLemmas) <- generatePreservationLemmas())
      lemmas(fn) ++= preservationLemmas
    lemmas.toMap
  }

  def makeHints(tag: Seq[String], fn: FunctionDef): Option[Hints] = Hints.fromDSK(problem, fn, tag)

  def makeProgressGraphConstructor(fn: FunctionDef): Option[GraphConstructor] = {
    makeHints(Seq("Progress"), fn).map(hints => new ProgressConstructor(problem, fn, hints))
  }

  def makePredicatePreservationGraphConstructor(fn: FunctionDef, predicate: FunctionDef): Option[GraphConstructor] = {
    makeHints(Seq("Preservation", predicate.signature.name), fn).map(hints =>
      new PredicatePreservationConstructor(problem, fn, predicate, hints))
  }

  def makeRelationalPreservationGraphConstructor(fn: FunctionDef,
                                                 relation: FunctionDef, termIndex: Int): Option[GraphConstructor] = {
    makeHints(Seq("Preservation", relation.signature.name, termIndex.toString), fn).map(hints =>
      new RelationalPreservationConstructor(problem, fn, relation, termIndex, hints))
  }
}
