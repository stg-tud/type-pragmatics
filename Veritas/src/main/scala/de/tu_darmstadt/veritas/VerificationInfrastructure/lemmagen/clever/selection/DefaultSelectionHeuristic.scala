package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.selection

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaEquivalence}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.{Inconclusive, RefinementGraph, RefinementNode}

import scala.collection.mutable

/** Select the lemmas that should be presented to the language designer, and set their selection status accordingly. */
class DefaultSelectionHeuristic extends SelectionHeuristic {
  /**
    * Return true if we can select a strict subset of premises of ``right`` such that
    * the resulting lemma is equivalent to ``left``. If this is the case,
    * ``left`` implies ``right``, and thus, ``left`` is (strictly) more general than ``right``.
    * */
  def moreGeneral(left: Lemma, right: Lemma): Boolean = {
    // if they have the same number of premises, or if left has more premises than right,
    // our heuristic can't return true
    if(left.premises.size >= right.premises.size)
      return false
    val rightPremisesSubsets = right.premises.toSet.subsets
    for(rightPremisesChoice <- rightPremisesSubsets if rightPremisesChoice.size == left.premises.size) {
      val moreGeneralRight = new Lemma(right.name, rightPremisesChoice.toSeq, right.consequences)
      if(LemmaEquivalence.isEquivalent(left, moreGeneralRight))
        return true
    }
    false
  }

  /** Given a set of nodes, remove all nodes for which `nodes` contains a more specific variant,
    * and return a new set. */
  def selectMostGeneralLemmas(nodes: Seq[RefinementNode]): Seq[RefinementNode] = {
    val remainingNodes = new mutable.HashSet[RefinementNode]()
    remainingNodes ++= nodes
    for(left <- nodes; right <- nodes)
      if(moreGeneral(left.lemma, right.lemma))
        remainingNodes.remove(right)
    remainingNodes.toSeq
  }

  def select(graph: RefinementGraph): Unit = {
    // get all inconclusive nodes
    val inconclusiveNodes = graph.collectNodes(Inconclusive())
    // only nodes that constrain all variables
    val filteredNodes = inconclusiveNodes.filter(node => node.lemma.boundVariables == node.constrainedVariables)
    // remove all nodes for which we can find predecessors in `nodes`
    val onlyPredecessors = filteredNodes.filterNot(node => {
      node.parents.exists(filteredNodes.contains)
    })
    // use ``moreGeneral`` to remove all nodes for which `nodes` contains a more specific variant.
    val remainingNodes = selectMostGeneralLemmas(onlyPredecessors.toSeq)
    if(remainingNodes.nonEmpty)
      for(node <- remainingNodes)
        node.select()
    else
      // if we have successfully removed all nodes, select the root node.
      graph.root.select()
  }
}
