package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma

trait Postprocessor {
  def process(lemmas: Seq[Lemma]): Seq[Lemma]
}
