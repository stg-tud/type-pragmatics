package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.backend.util.FreeVariables

class RankingHeuristic(graph: RefinementGraph) {
  def extract(): Seq[RefinementNode] = {
    // get all inconclusive nodes
    val inconclusiveNodes = graph.collectNodes(Inconclusive())
    // only nodes that constrain all variables
    val filteredNodes = inconclusiveNodes.filter(node => {
      val freePremiseVars = FreeVariables.freeVariables(node.lemma.premises)
      val freeConsequenceVars = FreeVariables.freeVariables(node.lemma.consequences) // TODO does not work for preservation
      freeConsequenceVars subsetOf freePremiseVars
    })
    val onlyDominators = filteredNodes.filterNot(node => {
      node.ancestors.exists(filteredNodes.contains)
    })
    for(node <- onlyDominators)
      node.selected = true
    onlyDominators.toSeq
  }
}
