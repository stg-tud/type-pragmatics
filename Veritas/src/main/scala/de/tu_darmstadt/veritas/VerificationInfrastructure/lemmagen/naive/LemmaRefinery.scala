package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}

import scala.collection.mutable

/** The "lemma refinery" constructs a pool of lemmas as follows:
  * Given a refinement strategy, it invokes `generateBase` to generate
  * the set of base lemmas, and adds it to the pool.
  * Then, it calls `expand` on all lemmas in the pool and
  * adds the resulting lemmas to the pool, unless an \approx-equivalent
  * lemma can already be found in the pool.
  *
  * The `LimitedDepthLemmaRefinery` implements a termination mechanism.
  */
class LemmaRefinery(problem: Problem, strategy: RefinementStrategy) {
  type LemmaGeneration = mutable.MutableList[Lemma]

  // Use a ShapedPool to be a bit faster
  protected val pool = new ShapedPool()

  def addChecked(generation: LemmaGeneration, lemma: Lemma): Unit = {
    if (pool.add(lemma))
      generation += lemma
  }

  def generate(): Seq[Lemma] = {
    var generation = new LemmaGeneration()
    // generate base, this is the first generation of lemmas
    strategy.generateBase().foreach(addChecked(generation, _))
    while (generation.nonEmpty) {
      val nextGeneration = new LemmaGeneration()
      // the next generation results from the current generation by
      // refining all lemmas in the current generation
      generation.foreach(lemma => {
        strategy.expand(lemma).foreach(refinement => {
          refinement.refine(problem, lemma).foreach(
            addChecked(nextGeneration, _)
          )
        })
      })
      println(s"next generation: ${nextGeneration.length} lemmas")
      generation = nextGeneration
    }
    pool.lemmas
  }
}