package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.postprocessing

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.RefinementGraph

trait Postprocessor {
  def process(lemmas: RefinementGraph): Seq[Lemma]
}
