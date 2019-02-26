package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaEquivalence, LemmaShape}

import scala.collection.mutable

class ShapedPool() {
  protected val pool: mutable.Map[LemmaShape, mutable.Set[Lemma]] =
    new mutable.HashMap[LemmaShape, mutable.Set[Lemma]]()

  def add(lemma: Lemma): Boolean = {
    val shape = lemma.shape()
    if(!pool.contains(shape))
      pool(shape) = new mutable.HashSet[Lemma]()
    val lemmaSet = pool(shape)
    if(!lemmaSet.contains(lemma)
      && !lemmaSet.exists(poolLemma => LemmaEquivalence.isEquivalent(poolLemma, lemma))) {
      lemmaSet += lemma
      true
    } else {
      false
    }
  }

  def lemmas: Seq[Lemma] = pool.valuesIterator.flatten.toSeq
}