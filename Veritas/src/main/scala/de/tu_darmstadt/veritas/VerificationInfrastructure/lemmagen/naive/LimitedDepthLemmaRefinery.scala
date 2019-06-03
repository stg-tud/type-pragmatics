package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}

/** This lemma refinery only refines lemmas that have at most `maxPremises` premises. */
class LimitedDepthLemmaRefinery(problem: Problem, strategy: RefinementStrategy, maxPremises: Int)
  extends LemmaRefinery(problem, strategy) {

  override def addChecked(generation: LemmaGeneration, lemma: Lemma): Unit = {
    if(pool.add(lemma) && lemma.premises.length < maxPremises)
      generation += lemma
  }
}
