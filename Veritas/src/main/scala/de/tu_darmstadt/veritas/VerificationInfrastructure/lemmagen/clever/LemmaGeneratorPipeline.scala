package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}
import de.tu_darmstadt.veritas.backend.ast.SortRef
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

import scala.collection.mutable

trait LemmaGeneratorPipeline {
  import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Query._

  def problem: Problem
  def makeProgressGraphConstructor(fn: FunctionDef): GraphConstructor
  def makePredicatePreservationGraphConstructor(fn: FunctionDef, predicate: FunctionDef): GraphConstructor
  def makeRelationalPreservationGraphConstructor(fn: FunctionDef, relation: FunctionDef,
                                                 termIndex: Int): GraphConstructor
  def makeOracleConsultation(): OracleConsultation
  def makeExtractionHeuristic(): ExtractionHeuristic
  def makePostProcessor(): Postprocessor

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

  def invokeConstructor(constructor: GraphConstructor): RefinementGraph = {
    constructor.construct()
  }

  def invokeOracle(graph: RefinementGraph): Unit = {
    makeOracleConsultation().consult(graph)
  }

  def invokeExtraction(graph: RefinementGraph): Unit = {
    makeExtractionHeuristic().extract(graph)
  }

  def invokePostprocessor(lemmas: Seq[Lemma]): Seq[Lemma] = {
    makePostProcessor().process(lemmas)
  }

  /*def generateWithConstructor(constructor: GraphConstructor,
                              directory: File): Seq[Lemma] = {
    if (!directory.exists())
      directory.mkdirs()
    val graph = constructor.construct()
    println(s"-- constructed graph with ${graph.nodes.size} nodes")
    graph.visualize(new File(directory, "step1.png"))
    val consultation = new VampireOracleConsultation(problem)
    consultation.consult(graph)
    println(s"-- consulted oracle")
    val extractor = new RankingHeuristic()
    graph.visualize(new File(directory, "step2.png"))
    val lemmas = extractor.extract(graph)
    println(s"-- extracted ${lemmas.size} lemmas")
    graph.visualize(new File(directory, "step3.png"))
    lemmas
  }*/

  def invokePipeline(constructor: GraphConstructor): Seq[Lemma] = {
    val graph = invokeConstructor(constructor)
    invokeOracle(graph)
    invokeExtraction(graph)
    val lemmas = graph.selectedNodes.map(_.lemma).toSeq
    invokePostprocessor(lemmas)
  }

  def generatePreservationLemmas(fn: FunctionDef): Seq[Lemma] = {
    val result = new mutable.HashSet[Lemma]()
    for(predicate <- getPredicatesInvolving(fn.successfulOutType)) {
      result ++= invokePipeline(makePredicatePreservationGraphConstructor(fn, predicate))
    }
    if(fn.inTypes.count(_ == fn.successfulOutType) >= 1) {
      for (relation <- getRelationsInvolving(fn.successfulOutType)) {
        val matchingIndices = fn.inTypes.zipWithIndex.collect {
          case (inType, idx) if inType == fn.successfulOutType => idx
        }
        for(idx <- matchingIndices) {
          result ++= invokePipeline(makeRelationalPreservationGraphConstructor(fn, relation, idx))
        }
      }
    }
    result.toSeq
  }


  def generateProgressLemmas(fn: FunctionDef): Seq[Lemma] = {
    invokePipeline(makeProgressGraphConstructor(fn))
  }

  def generatePreservationLemmas(): Map[FunctionDef, Seq[Lemma]] = {
    preservationFunctions.map(fn => fn -> generatePreservationLemmas(fn)).toMap
  }
}
