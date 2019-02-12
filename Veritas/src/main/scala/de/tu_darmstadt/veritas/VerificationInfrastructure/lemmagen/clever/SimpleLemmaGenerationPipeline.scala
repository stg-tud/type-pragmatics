package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma

class SimpleLemmaGenerationPipeline[Graph](constructor: GraphConstructor[Graph],
                                           consultation: OracleConsultation[Graph],
                                           extractor: ExtractionHeuristic[Graph]) extends LemmaGenerationPipeline {
  override def generate(): Seq[Lemma] = {
    val graph = constructor.construct()
    consultation.consult(graph)
    extractor.extract(graph)
  }
}
