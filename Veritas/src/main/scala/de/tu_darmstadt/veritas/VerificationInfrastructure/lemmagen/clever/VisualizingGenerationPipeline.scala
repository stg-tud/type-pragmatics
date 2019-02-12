package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever
import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma

class VisualizingGenerationPipeline(constructor: GraphConstructor[RefinementGraph],
                                    consultation: OracleConsultation[RefinementGraph],
                                    extractor: ExtractionHeuristic[RefinementGraph],
                                    directory: File) extends LemmaGenerationPipeline {

  if(!directory.exists())
    directory.mkdirs()

  override def generate(): Seq[Lemma] = {
    val graph = constructor.construct()
    println(s"-- constructed graph with ${graph.nodes.size} nodes")
    graph.visualize(new File(directory, "step1.png"))
    consultation.consult(graph)
    println(s"-- consulted oracle")
    graph.visualize(new File(directory, "step2.png"))
    val lemmas = extractor.extract(graph)
    println(s"-- extracted ${lemmas.size} lemmas")
    graph.visualize(new File(directory, "step3.png"))
    lemmas
  }
}
