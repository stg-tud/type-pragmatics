package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Problem
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction.GraphConstructor
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.selection.DefaultSelectionHeuristic
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.oracle.{DummyOracleConsultation, VampireOracleConsultation}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.postprocessing.DefaultPostprocessor
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

/** This class instantiates AbstractCleverLemmaGenerator with a Vampire oracle consultation step
  * and the default selection and postprocessing steps.
  * However, the lemma generator ignores all hints given in the specification.
  * This class implements the lemma generation algorithm Clever.
  */
class CleverLemmaGenerator(problem: Problem) extends AbstractCleverLemmaGenerator(problem) {
  val oracleConsultation = new VampireOracleConsultation(problem)
  val selectionHeuristic = new DefaultSelectionHeuristic()
  val postprocessor = new DefaultPostprocessor(problem)

  override def makeHints(tag: Seq[String], fn: FunctionDef): Option[Hints] = Some(Hints.empty(problem))

  override def makePipeline(constructor: GraphConstructor): LemmaGeneratorPipeline = {
    new LemmaGeneratorPipeline(
      constructor,
      oracleConsultation,
      selectionHeuristic,
      postprocessor
    )
  }
}
