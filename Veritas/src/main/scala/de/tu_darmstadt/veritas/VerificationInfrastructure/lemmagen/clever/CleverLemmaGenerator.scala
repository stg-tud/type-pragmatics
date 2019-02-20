package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.constructor.GraphConstructor

class CleverLemmaGenerator(val problem: Problem) extends AbstractLemmaGenerator {
  override def invokePipeline(constructor: GraphConstructor): Seq[Lemma] = {
    val pipeline = new DefaultGeneratorPipeline {
      override def problem: Problem = this.problem
      override def graphConstructor: GraphConstructor = constructor
    }
    pipeline.invokePipeline()
  }
}
