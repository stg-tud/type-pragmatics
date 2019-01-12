package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef, TypingRule, TypingRuleJudgment}
import de.tu_darmstadt.veritas.backend.util.FreeVariables

case class LemmaShape(premises: Set[TypingRuleJudgment], consequences: Set[TypingRuleJudgment])


class Lemma(name: String,
            premises: Seq[TypingRuleJudgment],
            consequences: Seq[TypingRuleJudgment],
            val refinements: Seq[Refinement] = Seq())
  extends TypingRule(name, premises, consequences) {
  require(consequences.length == 1, "only lemmas with a single consequence are supported")

  lazy val boundVariables: Set[MetaVar] = {
    FreeVariables.freeVariables(premises ++ consequences)
  }
  def bindings: Map[MetaVar, SortRef] = boundVariables.map(mv => (mv, mv.sortType)).toMap
  def boundTypes: Set[SortRef] = bindings.values.toSet
  def bindingsOfType(typ: SortRef): Set[MetaVar] = boundVariables.filter(_.sortType == typ).toSet

  def addPremise(refinement: Refinement, premise: TypingRuleJudgment): Lemma = {
    new Lemma(name, premises :+ premise, consequences, refinements :+ refinement)
  }

  def addPremise(premise: TypingRuleJudgment): Lemma = {
    new Lemma(name, premises :+ premise, consequences, refinements)
  }

  def shape(): LemmaShape = {
    LemmaShape(
      LemmaEquivalence.replaceVarsWithBottom(premises).toSet,
      LemmaEquivalence.replaceVarsWithBottom(consequences).toSet
    )
  }
}

object Lemma {
  def fromTypingRule(tr: TypingRule, refinements: Seq[Refinement] = Seq()): Lemma =
    new Lemma(tr.name, tr.premises, tr.consequences, refinements)
}