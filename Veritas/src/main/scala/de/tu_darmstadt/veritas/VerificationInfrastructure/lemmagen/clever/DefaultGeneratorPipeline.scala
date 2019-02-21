package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Problem
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.extraction.{DefaultHeuristic, ExtractionHeuristic}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.oracle.{OracleConsultation, VampireOracleConsultation}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.postprocessing.{DefaultPostprocessor, Postprocessor}

trait DefaultGeneratorPipeline extends LemmaGeneratorPipeline {
  def problem: Problem

  override def oracleConsultation: OracleConsultation = new VampireOracleConsultation(problem)

  override def extractionHeuristic: ExtractionHeuristic = new DefaultHeuristic()

  override def postProcessor: Postprocessor = new DefaultPostprocessor(problem)
}
