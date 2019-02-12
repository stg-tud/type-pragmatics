package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.backend.util.FreeVariables

class RankingHeuristic(graph: RefinementGraph) {
  def extract(): Seq[RefinementNode] = {
    // get all inconclusive nodes
    val inconclusiveNodes = graph.collectNodes(Inconclusive())
    // only nodes that constrain all variables
    val filteredNodes = inconclusiveNodes//.filter(node => node.lemma.boundVariables == node.constrainedVariables)
    val onlyDominators = filteredNodes.filterNot(node => {
      node.ancestors.exists(filteredNodes.contains)
    })
    for(node <- onlyDominators)
      node.selected = true
    onlyDominators.toSeq
  }
}
