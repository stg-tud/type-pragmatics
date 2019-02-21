package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Problem
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction.GraphConstructor
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.extraction.DefaultHeuristic
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.oracle.VampireOracleConsultation
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.postprocessing.DefaultPostprocessor

class CleverLemmaGenerator(problem: Problem) extends AbstractCleverLemmaGenerator(problem) {
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
