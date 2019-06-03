package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Problem
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction.GraphConstructor
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.oracle.VampireOracleConsultation
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.postprocessing.DefaultPostprocessor
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.selection.DefaultSelectionHeuristic
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

/** This class instantiates AbstractCleverLemmaGenerator with a Vampire oracle consultation step
  * and the default selection and postprocessing steps.
  * The lemma generator also processes all lemma generator hints of the specification.
  * This class implements the lemma generation algorithm CleverHints.
  */
class CleverHintsLemmaGenerator(problem: Problem) extends AbstractCleverLemmaGenerator(problem) {
  val oracleConsultation = new VampireOracleConsultation(problem)
  val selectionHeuristic = new DefaultSelectionHeuristic()
  val postprocessor = new DefaultPostprocessor(problem)

  override def makeHints(tag: Seq[String], fn: FunctionDef): Option[Hints] = Hints.fromDSK(problem, fn, tag)

  override def makePipeline(constructor: GraphConstructor): LemmaGeneratorPipeline = {
    new LemmaGeneratorPipeline(
      constructor,
      oracleConsultation,
      selectionHeuristic,
      postprocessor
    )
  }
}
