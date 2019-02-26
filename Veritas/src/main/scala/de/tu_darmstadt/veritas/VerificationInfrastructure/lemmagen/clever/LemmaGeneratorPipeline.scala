package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction.GraphConstructor
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.extraction.ExtractionHeuristic
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.oracle.OracleConsultation
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.postprocessing.Postprocessor
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma

class LemmaGeneratorPipeline(graphConstructor: GraphConstructor,
                             oracleConsultation: OracleConsultation,
                             extractionHeuristic: ExtractionHeuristic,
                             postprocessor: Postprocessor) {
  def invokeConstructor(): RefinementGraph = {
    graphConstructor.construct()
  }

  def invokeOracle(graph: RefinementGraph): Unit = {
    oracleConsultation.consult(graph)
  }

  def invokeExtraction(graph: RefinementGraph): Unit = {
    extractionHeuristic.extract(graph)
  }

  def invokePostprocessor(graph: RefinementGraph): Seq[Lemma] = {
    postprocessor.process(graph)
  }

  def invokePipeline(): Seq[Lemma] = {
    val graph = invokeConstructor()
    invokeOracle(graph)
    invokeExtraction(graph)
    invokePostprocessor(graph)
  }
}
