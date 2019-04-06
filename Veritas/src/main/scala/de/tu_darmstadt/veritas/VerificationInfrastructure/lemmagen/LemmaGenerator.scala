package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

import scala.collection.mutable

/** Abstract trait for lemma generators for progress and preservation lemmas.
  * This trait is implemented by `NaiveLemmaGenerator`, `CleverLemmaGenerator`
  * and `CleverHintsLemmaGenerator`.
  */
trait LemmaGenerator {
  def generateProgressLemmas(): Map[FunctionDef, Seq[Lemma]]
  def generatePreservationLemmas(): Map[FunctionDef, Seq[Lemma]]

  /** Return a map of functions with sequences of generated progress and preservation lemmas for this function. */
  def generateLemmas(): Map[FunctionDef, Seq[Lemma]] = {
    val generatedLemmas = new mutable.HashMap[FunctionDef, Seq[Lemma]]().withDefaultValue(Seq())
    for((fn, lemmas) <- generateProgressLemmas())
      generatedLemmas(fn) ++= lemmas
    for((fn, lemmas) <- generatePreservationLemmas())
      generatedLemmas(fn) ++= lemmas
    generatedLemmas.toMap
  }
}
