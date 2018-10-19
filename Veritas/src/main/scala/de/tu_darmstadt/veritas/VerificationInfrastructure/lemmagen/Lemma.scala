package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef, TypingRule, TypingRuleJudgment}

class Lemma(val bindings: Map[MetaVar, SortRef], val rule: TypingRule) {
  def boundTypes: Set[SortRef] = bindings.values.toSet
  def bind(variables: MetaVar*) = new Lemma(bindings ++ variables.map(v => (v, v.sortType)), rule)
  def withPremise(premise: TypingRuleJudgment) =
    new Lemma(
      bindings,
      TypingRule(rule.name, premise +: rule.premises, rule.consequences)
    )
}

object Lemma {

}