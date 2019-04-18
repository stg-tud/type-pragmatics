package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.selection

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.RefinementGraph

trait SelectionHeuristic {
  def select(graph: RefinementGraph): Unit
}
