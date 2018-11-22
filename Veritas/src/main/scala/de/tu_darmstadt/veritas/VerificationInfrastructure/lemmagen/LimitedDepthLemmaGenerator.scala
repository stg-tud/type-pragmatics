package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

class LimitedDepthLemmaGenerator(problem: Problem, strategy: RefinementStrategy, maxPremises: Int)
  extends LemmaGenerator(problem, strategy) {

  override def addChecked(generation: LemmaGeneration, lemma: Lemma): Unit = {
    if(pool.add(lemma) && lemma.premises.length < maxPremises)
      generation += lemma
  }
}
