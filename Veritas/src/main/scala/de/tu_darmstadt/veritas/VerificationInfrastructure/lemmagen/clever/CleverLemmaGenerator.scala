package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.constructor.GraphConstructor

class CleverLemmaGenerator(val problem: Problem) extends AbstractLemmaGenerator {
  override def makePipeline(constructor: GraphConstructor): LemmaGeneratorPipeline = {
    val thisProblem = problem
    new DefaultGeneratorPipeline {
      override def problem: Problem = thisProblem
      override def graphConstructor: GraphConstructor = constructor
    }
  }
}
