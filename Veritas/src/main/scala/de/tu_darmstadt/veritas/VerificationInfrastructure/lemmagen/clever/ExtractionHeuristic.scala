package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma

trait ExtractionHeuristic {
  def extract(graph: RefinementGraph): Seq[Lemma]
}
