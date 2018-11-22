package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef, TypingRule, TypingRuleJudgment}
import de.tu_darmstadt.veritas.backend.util.FreeVariables

case class LemmaShape(premises: Set[TypingRuleJudgment], consequences: Set[TypingRuleJudgment])


class Lemma(name: String, premises: Seq[TypingRuleJudgment], consequences: Seq[TypingRuleJudgment])
  extends TypingRule(name, premises, consequences) {

  lazy val freeVariables: Set[MetaVar] = {
    FreeVariables.freeVariables(premises ++ consequences)
  }
  def bindings: Map[MetaVar, SortRef] = freeVariables.map(mv => (mv, mv.sortType)).toMap
  def boundTypes: Set[SortRef] = bindings.values.toSet
  def bindingsOfType(typ: SortRef): Set[MetaVar] = freeVariables.filter(_.sortType == typ).toSet

  def addPremise(premise: TypingRuleJudgment): Lemma = {
    new Lemma(name, premises :+ premise, consequences)
  }

  def shape(): LemmaShape = {
    LemmaShape(
      LemmaEquivalence.replaceVarsWithBottom(premises).toSet,
      LemmaEquivalence.replaceVarsWithBottom(consequences).toSet
    )
  }
}
