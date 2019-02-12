package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma

class RankingHeuristic extends ExtractionHeuristic[RefinementGraph] {
  def extract(graph: RefinementGraph): Seq[Lemma] = {
    // get all inconclusive nodes
    val inconclusiveNodes = graph.collectNodes(Inconclusive())
    // only nodes that constrain all variables
    val filteredNodes = inconclusiveNodes.filter(node => node.lemma.boundVariables == node.constrainedVariables)
    val onlyDominators = filteredNodes.filterNot(node => {
      node.ancestors.exists(filteredNodes.contains)
    })
    for(node <- onlyDominators)
      node.selected = true
    onlyDominators.map(_.lemma).toSeq
  }
}
