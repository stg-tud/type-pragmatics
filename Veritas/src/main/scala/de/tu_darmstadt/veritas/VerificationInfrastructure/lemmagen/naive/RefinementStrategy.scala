package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Refinement}

/** A refinement strategy encapsulates the base generation and expansion of lemmas.
  */
trait RefinementStrategy {
  def generateBase(): Seq[Lemma]
  def expand(lemma: Lemma): Seq[Refinement]
}
