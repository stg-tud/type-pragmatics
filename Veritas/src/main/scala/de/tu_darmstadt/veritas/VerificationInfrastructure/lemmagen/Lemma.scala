package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.{MetaVar, SortRef, TypingRule, TypingRuleJudgment}
import de.tu_darmstadt.veritas.backend.util.FreeVariables

/** Container for lemma shapes, i.e. with all variables replaced with LemmaEquivalence.Bottom */
case class LemmaShape(premises: Set[TypingRuleJudgment], consequences: Set[TypingRuleJudgment])


/** Our Lemma class, an immutable TypingRule with a few helper methods. */
class Lemma(name: String,
            premises: Seq[TypingRuleJudgment],
            consequences: Seq[TypingRuleJudgment])
  extends TypingRule(name, premises, consequences) {
  require(consequences.length == 1, "only lemmas with a single consequence are supported")

  lazy val boundVariables: Set[MetaVar] = {
    FreeVariables.freeVariables(premises ++ consequences)
  }
  def bindings: Map[MetaVar, SortRef] = boundVariables.map(mv => (mv, mv.sortType)).toMap
  def boundTypes: Set[SortRef] = bindings.values.toSet
  def bindingsOfType(typ: SortRef): Set[MetaVar] = boundVariables.filter(_.sortType == typ)

  /** Return a new lemma with an extra premise */
  def addPremise(premise: TypingRuleJudgment): Lemma = {
    new Lemma(name, premises :+ premise, consequences)
  }

  /** Replace all variables with LemmaEquivalence.bottom */
  def shape(): LemmaShape = {
    LemmaShape(
      LemmaEquivalence.replaceVarsWithBottom(premises).toSet,
      LemmaEquivalence.replaceVarsWithBottom(consequences).toSet
    )
  }

  def rename(newName: String): Lemma =
    new Lemma(newName, premises, consequences)
}

object Lemma {
  def fromTypingRule(tr: TypingRule): Lemma =
    new Lemma(tr.name, tr.premises, tr.consequences)
}