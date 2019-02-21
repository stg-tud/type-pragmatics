package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Refinement}

trait RefinementStrategy {
  def generateBase(): Seq[Lemma]
  def expand(lemma: Lemma): Seq[Refinement]
}
