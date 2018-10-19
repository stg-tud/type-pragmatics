package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef, TypingRule}

class Lemma(val bindings: Map[MetaVar, SortRef], val rule: TypingRule) {
  def boundTypes: Set[SortRef] = bindings.values.toSet
  def bind(variable: MetaVar, typ: SortRef) = new Lemma(bindings + (variable -> typ), rule)
}

object Lemma {

}