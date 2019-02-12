package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma

trait LemmaGenerationPipeline {
  def generate(): Seq[Lemma]
}
