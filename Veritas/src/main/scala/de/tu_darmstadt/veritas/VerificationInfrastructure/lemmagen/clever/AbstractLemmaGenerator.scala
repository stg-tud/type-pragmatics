package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.constructor.{GraphConstructor, PredicatePreservationConstructor, ProgressConstructor, RelationalPreservationConstructor}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}
import de.tu_darmstadt.veritas.backend.ast.SortRef
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

import scala.collection.mutable

trait AbstractLemmaGenerator {
  def problem: Problem
  def makePipeline(constructor: GraphConstructor): LemmaGeneratorPipeline

  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Query._
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
    for(predicate <- getPredicatesInvolving(fn.successfulOutType)) {
      result ++= makePipeline(makePredicatePreservationGraphConstructor(fn, predicate)).invokePipeline()
    }
    if(fn.inTypes.count(_ == fn.successfulOutType) >= 1) {
      for (relation <- getRelationsInvolving(fn.successfulOutType)) {
        val matchingIndices = fn.inTypes.zipWithIndex.collect {
          case (inType, idx) if inType == fn.successfulOutType => idx
        }
        for(idx <- matchingIndices) {
          result ++= makePipeline(makeRelationalPreservationGraphConstructor(fn, relation, idx)).invokePipeline()
        }
      }
    }
    result.toSeq
  }

  def generatePreservationLemmas(): Map[FunctionDef, Seq[Lemma]] = {
    preservationFunctions.map(fn => fn -> generatePreservationLemmas(fn)).toMap
  }

  def generateProgressLemmas(fn: FunctionDef): Seq[Lemma] = {
    makePipeline(makeProgressGraphConstructor(fn)).invokePipeline()
  }

  def generateProgressLemmas(): Map[FunctionDef, Seq[Lemma]] = {
    progressFunctions.map(fn => fn -> generateProgressLemmas(fn)).toMap
  }

  def generate(): Map[FunctionDef, Seq[Lemma]] = {
    val lemmas = new mutable.HashMap[FunctionDef, Seq[Lemma]]()
    for((fn, progressLemmas) <- generateProgressLemmas())
      lemmas(fn) = progressLemmas
    for((fn, preservationLemmas) <- generatePreservationLemmas())
      lemmas(fn) ++= preservationLemmas
    Map()
  }

  def makeHints(tag: String, fn: FunctionDef): Hints = Hints.fromDSK(problem, fn, tag)

  def makeProgressGraphConstructor(fn: FunctionDef): GraphConstructor = {
    new ProgressConstructor(problem, fn,
      makeHints(s"progress", fn))
  }

  def makePredicatePreservationGraphConstructor(fn: FunctionDef, predicate: FunctionDef): GraphConstructor = {
    new PredicatePreservationConstructor(problem, fn, predicate,
      makeHints(s"preservation/predicate/${predicate.signature.name}", fn))
  }

  def makeRelationalPreservationGraphConstructor(fn: FunctionDef,
                                                 relation: FunctionDef, termIndex: Int): GraphConstructor = {
    new RelationalPreservationConstructor(problem, fn, relation, termIndex,
      makeHints(s"preservation/relational/${relation.signature.name}/$termIndex", fn))
  }
}
