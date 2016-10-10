package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast._

/**
  * DSL for axioms, lemmas, goals, and other statements used for proofs
  * currently does not allow for axiom/lemma/goal blocks with multiple typing rules
  * //TODO is this needed, on DSL level?
  */
object ProofDSL {
  def axiom(ax: TypingRule) = Axioms(Seq(ax))

  def lemma(lem: TypingRule) = Lemmas(Seq(lem), None)

  def lemma(timeout: Int, lem: TypingRule) = Lemmas(Seq(lem), Some(timeout))

  def goal(g: TypingRule) = Goals(Seq(g), None)

  def goal(timeout: Int, g: TypingRule) = Goals(Seq(g), Some(timeout))

  def local(mds: Seq[ModuleDef]) = Local(mds)
}
