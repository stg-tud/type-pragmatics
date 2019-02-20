package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma

class DefaultHeuristic extends ExtractionHeuristic {
  def extract(graph: RefinementGraph): Unit = {
    // get all inconclusive nodes
    val inconclusiveNodes = graph.collectNodes(Inconclusive())
    // only nodes that constrain all variables
    val filteredNodes = inconclusiveNodes.filter(node => node.lemma.boundVariables == node.constrainedVariables)
    val onlyDominators = filteredNodes.filterNot(node => {
      node.ancestors.exists(filteredNodes.contains)
    })
    for(node <- onlyDominators)
      node.select()
  }
}
