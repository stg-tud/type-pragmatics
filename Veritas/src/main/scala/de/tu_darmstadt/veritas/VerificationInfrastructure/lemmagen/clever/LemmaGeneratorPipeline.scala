package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction.GraphConstructor
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.selection.SelectionHeuristic
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.oracle.OracleConsultation
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.postprocessing.Postprocessor
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma

/** A so-called lemma generator pipeline encapsulates the following steps of lemma generation:
  * 1) refinement graph constructrion
  * 2) oracle consultation
  * 3) lemma selection
  * 4) postprocessing
  * The method `invokePipeline` invokes all steps in the correct order
  * and returns a sequence of generated lemmas.
  */
class LemmaGeneratorPipeline(graphConstructor: GraphConstructor,
                             oracleConsultation: OracleConsultation,
                             selectionHeuristic: SelectionHeuristic,
                             postprocessor: Postprocessor) {
  def invokeConstructor(): RefinementGraph = {
    graphConstructor.construct()
  }

  def invokeOracle(graph: RefinementGraph): Unit = {
    oracleConsultation.consult(graph)
  }

  def invokeSelection(graph: RefinementGraph): Unit = {
    selectionHeuristic.select(graph)
  }

  def invokePostprocessor(graph: RefinementGraph): Seq[Lemma] = {
    postprocessor.process(graph)
  }

  def invokePipeline(): Seq[Lemma] = {
    val graph = invokeConstructor()
    invokeOracle(graph)
    invokeSelection(graph)
    invokePostprocessor(graph)
  }
}
