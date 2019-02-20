package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}
import de.tu_darmstadt.veritas.backend.ast.SortRef
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

import scala.collection.mutable

class CleverLemmaGenerator(problem: Problem, pipeline: LemmaGeneratorPipeline) {
  def this(problem: Problem) {
    this(problem, new DefaultGeneratorPipeline(problem))
  }

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
      result ++= pipeline.invokePipeline(pipeline.makePredicatePreservationGraphConstructor(fn, predicate))
    }
    if(fn.inTypes.count(_ == fn.successfulOutType) >= 1) {
      for (relation <- getRelationsInvolving(fn.successfulOutType)) {
        val matchingIndices = fn.inTypes.zipWithIndex.collect {
          case (inType, idx) if inType == fn.successfulOutType => idx
        }
        for(idx <- matchingIndices) {
          result ++= pipeline.invokePipeline(pipeline.makeRelationalPreservationGraphConstructor(fn, relation, idx))
        }
      }
    }
    result.toSeq
  }

  def generatePreservationLemmas(): Map[FunctionDef, Seq[Lemma]] = {
    preservationFunctions.map(fn => fn -> generatePreservationLemmas(fn)).toMap
  }

  def generateProgressLemmas(fn: FunctionDef): Seq[Lemma] = {
    pipeline.invokePipeline(pipeline.makeProgressGraphConstructor(fn))
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
}
