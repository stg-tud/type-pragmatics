package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExp, FunctionExpMeta}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.inputdsl.FunctionDSL.FunExpMetaTree

/**
  * DSL for creating typing rule constructs
  */
object TypingRuleDSL {

  //support for typing rules without premises
  def ===>(name: String) = _Concmissing(name)

  case class _Concmissing(name: String) {
    def apply(conc: TypingRuleJudgment): TypingRule = TypingRule(name, Seq(), Seq(conc))
    def apply(concs: Seq[TypingRuleJudgment]): TypingRule = TypingRule(name, Seq(), concs)
  }

  //support for typing rules with just one premise
  implicit class _TypingRulePartial(prem: TypingRuleJudgment) {
    def ===>(name: String) = _Concmissing(name)

    case class _Concmissing(name: String) {
      def apply(conc: TypingRuleJudgment): TypingRule = TypingRule(name, Seq(prem), Seq(conc))
      def apply(concs: Seq[TypingRuleJudgment]): TypingRule = TypingRule(name, Seq(prem), concs)
    }
  }

  //support for typing rules with multiple premises
  implicit class _TypingRulePartialSeq(prems: Seq[TypingRuleJudgment]) {
    def ===>(name: String) = _Concmissing(name)

    case class _Concmissing(name: String) {
      def apply(conc: TypingRuleJudgment): TypingRule = TypingRule(name, prems, Seq(conc))
      def apply(concs: Seq[TypingRuleJudgment]): TypingRule = TypingRule(name, prems, concs)
    }
  }

}

/**
  * DSL for creating TypingRuleJudgment constructs
  */
object TypingRuleJudgmentDSL {

  import SymTreeDSL._
  import FunctionDSL._

  implicit class _PartialTypingJudgmentSym(f1: Symbol) {
    def :: (f2: FunExpMetaTree) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f1), _funExpMetaTreeToFunExpMeta(f2))
    def |- (ts: TypingJudgmentSimple) = TypingJudgment(_funExpMetaTreeToFunExpMeta(f1), ts.f1, ts.f2)
  }

  implicit class _PartialTypingJudgmentST(f1: SymTree) {
    def :: (f2: FunExpMetaTree) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f1), _funExpMetaTreeToFunExpMeta(f2))
    def |- (ts: TypingJudgmentSimple) = TypingJudgment(_funExpTreeToFunExp(f1), ts.f1, ts.f2)
  }

  implicit class _PartialTypingJudgment(f1: FunExpMetaTree) {
    def :: (f2: FunExpMetaTree) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f1), _funExpMetaTreeToFunExpMeta(f2))
    def |- (ts: TypingJudgmentSimple) = TypingJudgment(_funExpMetaTreeToFunExpMeta(f1), ts.f1, ts.f2)
  }

  implicit def _toFunctionExpJudgment(f: FunctionExp) = FunctionExpJudgment(f)

  implicit def _toFunctionExpJudgment(f: FunExpTree) = FunctionExpJudgment(_funExpTreeToFunExp(f))

  def exists(mvs: MetaVar*) = _existsJudgmentPartial(mvs)

  case class _existsJudgmentPartial(mvs: Seq[MetaVar]) {
    def |(jdg: TypingRuleJudgment) = ExistsJudgment(mvs, Seq(jdg))
    def |(jdglist: Seq[TypingRuleJudgment]) = ExistsJudgment(mvs, jdglist)
  }

  def forall(mvs: MetaVar*) = _forallJudgmentPartial(mvs)

  case class _forallJudgmentPartial(mvs: Seq[MetaVar]) {
    def |(jdg: TypingRuleJudgment) = ForallJudgment(mvs, Seq(jdg))
    def |(jdglist: Seq[TypingRuleJudgment]) = ForallJudgment(mvs, jdglist)
  }

  def !(jdg: TypingRuleJudgment) = NotJudgment(jdg)

  //support for generating sequences of TypingRuleJudgments
  implicit class _toTypingRuleJudgmentSeqSingle(jdg: TypingRuleJudgment) {
    def &&(next: TypingRuleJudgment): Seq[TypingRuleJudgment] = Seq(jdg) :+ next
  }

  implicit class _toTypingRuleJudgmentSeq(jdgs: Seq[TypingRuleJudgment]) {
    def &&(next: TypingRuleJudgment): Seq[TypingRuleJudgment] = jdgs :+ next
  }

  def OR(orcases: Seq[Seq[TypingRuleJudgment]]) = OrJudgment(orcases)

  def =>> (tjdg: TypingRuleJudgment) = Seq(Seq(tjdg))
  def =>> (tjdgs: Seq[TypingRuleJudgment]) = Seq(tjdgs)

  implicit class _appendOrcase(orcases: Seq[Seq[TypingRuleJudgment]]) {
    def |(next: Seq[Seq[TypingRuleJudgment]]) = orcases ++ next
  }
}
