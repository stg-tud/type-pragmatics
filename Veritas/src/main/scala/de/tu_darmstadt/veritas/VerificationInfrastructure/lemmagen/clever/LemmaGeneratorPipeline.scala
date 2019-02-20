package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.constructor.GraphConstructor
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.extraction.ExtractionHeuristic
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.oracle.OracleConsultation
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.postprocessor.Postprocessor
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}
import de.tu_darmstadt.veritas.backend.ast.SortRef
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

trait LemmaGeneratorPipeline {

  def problem: Problem
  def makeProgressGraphConstructor(fn: FunctionDef): GraphConstructor
  def makePredicatePreservationGraphConstructor(fn: FunctionDef, predicate: FunctionDef): GraphConstructor
  def makeRelationalPreservationGraphConstructor(fn: FunctionDef, relation: FunctionDef,
                                                 termIndex: Int): GraphConstructor
  def makeOracleConsultation(): OracleConsultation
  def makeExtractionHeuristic(): ExtractionHeuristic
  def makePostProcessor(): Postprocessor



  def invokeConstructor(constructor: GraphConstructor): RefinementGraph = {
    constructor.construct()
  }

  def invokeOracle(graph: RefinementGraph): Unit = {
    makeOracleConsultation().consult(graph)
  }

  def invokeExtraction(graph: RefinementGraph): Unit = {
    makeExtractionHeuristic().extract(graph)
  }

  def invokePostprocessor(graph: RefinementGraph): Seq[Lemma] = {
    makePostProcessor().process(graph)
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
    invokePostprocessor(graph)
  }


}
