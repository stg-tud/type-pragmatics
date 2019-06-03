package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.selection

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.RefinementGraph

/** Abstract trait for lemma selection. */
trait SelectionHeuristic {
  def select(graph: RefinementGraph): Unit
}
