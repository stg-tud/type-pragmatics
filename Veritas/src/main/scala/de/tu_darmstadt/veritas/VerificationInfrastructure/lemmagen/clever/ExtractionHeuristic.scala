package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma

trait ExtractionHeuristic[Graph] {
  def extract(graph: Graph): Seq[Lemma]
}
