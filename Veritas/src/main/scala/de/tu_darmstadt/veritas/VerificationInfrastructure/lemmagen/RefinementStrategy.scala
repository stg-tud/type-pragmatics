package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

trait RefinementStrategy {
  def generateBase(): Seq[Lemma]
  def expand(lemma: Lemma): Seq[Refinement]
}
