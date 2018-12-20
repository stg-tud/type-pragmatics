package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import scala.collection.mutable

class LemmaGenerator(problem: Problem, strategy: RefinementStrategy) {
  type LemmaGeneration = mutable.MutableList[Lemma]

  protected val pool = new ShapedPool()

  def addChecked(generation: LemmaGeneration, lemma: Lemma): Unit = {
    if (pool.add(lemma))
      generation += lemma
  }

  def generate(): Seq[Lemma] = {
    var generation = new LemmaGeneration()
    strategy.generateBase().foreach(addChecked(generation, _))
    while (generation.nonEmpty) {
      val nextGeneration = new LemmaGeneration()
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