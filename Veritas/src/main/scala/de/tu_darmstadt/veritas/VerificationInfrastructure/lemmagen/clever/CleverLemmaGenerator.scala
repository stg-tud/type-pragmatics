package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Problem
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction.GraphConstructor
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.selection.DefaultHeuristic
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.oracle.{DummyOracleConsultation, VampireOracleConsultation}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.postprocessing.DefaultPostprocessor

class CleverLemmaGenerator(problem: Problem) extends AbstractCleverLemmaGenerator(problem) {
  val oracleConsultation = new VampireOracleConsultation(problem)
  val selectionHeuristic = new DefaultHeuristic()
  val postprocessor = new DefaultPostprocessor(problem)

  override def makePipeline(constructor: GraphConstructor): LemmaGeneratorPipeline = {
    new LemmaGeneratorPipeline(
      constructor,
      oracleConsultation,
      selectionHeuristic,
      postprocessor
    )
  }
}
