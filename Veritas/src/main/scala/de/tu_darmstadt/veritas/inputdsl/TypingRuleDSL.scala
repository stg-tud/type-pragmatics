package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExp
import de.tu_darmstadt.veritas.inputdsl.FunctionDSL.FunExpTree
import de.tu_darmstadt.veritas.inputdsl.SymTreeDSL.SymTree

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


  implicit class _TypingRulePartialFET(prem: FunExpTree) {
    def ===>(name: String) = _Concmissing(name)

    case class _Concmissing(name: String) {
      def apply(conc: TypingRuleJudgment): TypingRule = TypingRule(name, Seq(_toFunctionExpJudgment(prem)), Seq(conc))
      def apply(concs: Seq[TypingRuleJudgment]): TypingRule = TypingRule(name, Seq(_toFunctionExpJudgment(prem)), concs)
    }
  }

  implicit class _TypingRulePartialST(prem: SymTree) {
    def ===>(name: String) = _Concmissing(name)

    case class _Concmissing(name: String) {
      def apply(conc: TypingRuleJudgment): TypingRule = TypingRule(name, Seq(_toFunctionExpJudgment(prem)), Seq(conc))
      def apply(concs: Seq[TypingRuleJudgment]): TypingRule = TypingRule(name, Seq(_toFunctionExpJudgment(prem)), concs)
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

  /* //This would be needed if complex function expressions shall be supported as arguments of typing judgments
  // however, it would not directly work: there would be ambiguities with implicit class _PartialTypingJudgmentST(f1: SymTree)
  implicit class _PartialTypingJudgment(f1: FunExpMetaTree) {
    def :: (f2: FunExpMetaTree) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f1), _funExpMetaTreeToFunExpMeta(f2))
    def |- (ts: TypingJudgmentSimple) = TypingJudgment(_funExpMetaTreeToFunExpMeta(f1), ts.f1, ts.f2)
  }*/

  implicit def _toFunctionExpJudgment(f: FunctionExp): FunctionExpJudgment = FunctionExpJudgment(f)

  implicit def _toFunctionExpJudgment(f: FunExpTree): FunctionExpJudgment = FunctionExpJudgment(_funExpTreeToFunExp(f))

  def exists(mvs: MVarNode*) = _existsJudgmentPartial(mvs map {_.mv})

  case class _existsJudgmentPartial(mvs: Seq[MetaVar]) {
    def |(jdg: TypingRuleJudgment) = ExistsJudgment(mvs, Seq(jdg))
    def |(jdglist: Seq[TypingRuleJudgment]) = ExistsJudgment(mvs, jdglist)
  }

  def forall(mvs: MVarNode*) = _forallJudgmentPartial(mvs map {_.mv})

  case class _forallJudgmentPartial(mvs: Seq[MetaVar]) {
    def |(jdg: TypingRuleJudgment) = ForallJudgment(mvs, Seq(jdg))
    def |(jdglist: Seq[TypingRuleJudgment]) = ForallJudgment(mvs, jdglist)
  }

  def !(jdg: TypingRuleJudgment) = NotJudgment(jdg)

  //support for generating sequences of TypingRuleJudgments
  //also has to support direct generation from FunExpTree
  // (important: NOT from FunExpMetaTree, single MetaVars as typing judgments would not be valid!)
  implicit class _toTypingRuleJudgmentSeqSingle(jdg: TypingRuleJudgment) {
    def &(next: TypingRuleJudgment): Seq[TypingRuleJudgment] = Seq(jdg) :+ next
    def &(next: FunExpTree): Seq[TypingRuleJudgment] = Seq(jdg) :+ _toFunctionExpJudgment(next)
  }

  implicit class _toTypingRuleJudgmentSeqSingleFET(jdg: FunExpTree) {
    def &(next: TypingRuleJudgment): Seq[TypingRuleJudgment] = Seq(_toFunctionExpJudgment(jdg)) :+ next
    def &(next: FunExpTree): Seq[TypingRuleJudgment] = Seq(_toFunctionExpJudgment(jdg)) :+ _toFunctionExpJudgment(next)
  }

  implicit class _toTypingRuleJudgmentSeq(jdgs: Seq[TypingRuleJudgment]) {
    def &(next: TypingRuleJudgment): Seq[TypingRuleJudgment] = jdgs :+ next
    def &(next: FunExpTree): Seq[TypingRuleJudgment] = jdgs :+ _toFunctionExpJudgment(next)
  }

  def OR(orcases: Seq[Seq[TypingRuleJudgment]]) = OrJudgment(orcases)

  def =>> (tjdg: TypingRuleJudgment) = Seq(Seq(tjdg))
  def =>> (tjdgs: Seq[TypingRuleJudgment]) = Seq(tjdgs)

  implicit class _appendOrcase(orcases: Seq[Seq[TypingRuleJudgment]]) {
    def |(next: Seq[Seq[TypingRuleJudgment]]) = orcases ++ next
  }
}
