package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction.GraphConstructor
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.extraction.DefaultHeuristic
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.oracle.{OracleConsultation, VampireOracleConsultation}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.postprocessing.{DefaultPostprocessor, Postprocessor}

class CleverLemmaGenerator(problem: Problem) extends AbstractLemmaGenerator(problem) {
  val oracleConsultation = new VampireOracleConsultation(problem)
  val extractionHeuristic = new DefaultHeuristic()
  val postprocessor = new DefaultPostprocessor(problem)

  override def makePipeline(constructor: GraphConstructor): LemmaGeneratorPipeline = {
    new LemmaGeneratorPipeline(
      constructor,
      oracleConsultation,
      extractionHeuristic,
      postprocessor
    )
  }
}
