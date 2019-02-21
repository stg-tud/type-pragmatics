package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.{AnnotatedLemma, RefinementGraph, RefinementNode}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Problem, Refinement}

trait GraphConstructor {
  def problem: Problem
  def constructRoot(): AnnotatedLemma
  def expand(node: RefinementNode): Set[Refinement]

  def construct(): RefinementGraph = {
    val root = new RefinementNode(constructRoot())
    val graph = new RefinementGraph(problem, root)
    while(graph.openNodes.nonEmpty) {
      for(node <- graph.openNodes) {
        val restrictions = expand(node)
        for (restriction <- restrictions) {
          graph.refine(node, restriction)
        }
        node.open = false
      }
    }
    graph
  }
}
