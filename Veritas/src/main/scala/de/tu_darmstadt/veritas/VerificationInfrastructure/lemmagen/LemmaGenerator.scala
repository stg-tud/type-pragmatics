package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef

import scala.collection.mutable

trait LemmaGenerator {
  def generateProgressLemmas(): Map[FunctionDef, Seq[Lemma]]
  def generatePreservationLemmas(): Map[FunctionDef, Seq[Lemma]]

  def generateLemmas(): Map[FunctionDef, Seq[Lemma]] = {
    val generatedLemmas = new mutable.HashMap[FunctionDef, Seq[Lemma]]().withDefaultValue(Seq())
    for((fn, lemmas) <- generateProgressLemmas())
      generatedLemmas(fn) ++= lemmas
    for((fn, lemmas) <- generatePreservationLemmas())
      generatedLemmas(fn) ++= lemmas
    generatedLemmas.toMap
  }
}
