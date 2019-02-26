package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.extraction

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.RefinementGraph

trait ExtractionHeuristic {
  def extract(graph: RefinementGraph): Unit
}
