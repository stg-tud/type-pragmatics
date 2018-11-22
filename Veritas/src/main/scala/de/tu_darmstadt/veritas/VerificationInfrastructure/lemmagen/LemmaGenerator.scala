package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import scala.collection.mutable

class LemmaGenerator(problem: Problem, strategy: RefinementStrategy) {
  type LemmaGeneration = mutable.MutableList[Lemma]

  protected val pool = new ShapedPool()

  def addChecked(generation: LemmaGeneration, lemma: Lemma): Unit = {
    if(pool.add(lemma))
      generation += lemma
  }

  def generate(): Seq[Lemma] = {
    var generation = new LemmaGeneration()
    generation ++= strategy.generateBase() // TODO: Do we need to add them to the pool?
    while(generation.nonEmpty) {
      val nextGeneration = new LemmaGeneration()
      generation.foreach(lemma => {
        strategy.expand(lemma).foreach(refinement => {
          refinement.refine(problem, lemma).foreach(
            addChecked(nextGeneration, _)
          )
        })
      })
      generation = nextGeneration
    }
    pool.lemmas
  }
}
