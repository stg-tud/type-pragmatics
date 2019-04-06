package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaEquivalence, LemmaShape}

import scala.collection.mutable

/** A shaped pool holds a set of lemmas. It provides a method `add`
  * that adds a lemma to the pool, except if the pool already contains an \approx-equivalent lemma.
  * In order to implement this check more efficiently, we store each lemma along with its "shape",
  * i.e. the lemma with all variables replaced with LemmaEquivalence.bottom.
  * When adding a new lemma, we only check \approx-equivalence for all lemmas with the same shape.
  */
class ShapedPool() {
  protected val pool: mutable.Map[LemmaShape, mutable.Set[Lemma]] =
    new mutable.HashMap[LemmaShape, mutable.Set[Lemma]]()

  /** Add a lemma to the pool. Return true if it was actually added, return false if
    * the pool already contains an \approx-equivalent lemma.
    */
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

  /** Return the stored set of lemmas. */
  def lemmas: Seq[Lemma] = pool.valuesIterator.flatten.toSeq
}