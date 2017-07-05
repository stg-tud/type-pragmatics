package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExp

import scala.language.implicitConversions

/**
  * DSL for creating typing rule constructs
  */
object TypingRuleDSL {

  import SymTreeDSL._
  import FunctionDSL._

  //support for typing rules without premises
  def ===>(name: String): _Concmissing = _Concmissing(name)

  case class _Concmissing(name: String) {
    def apply(conc: TypingRuleJudgment): TypingRule = TypingRule(name, Seq(), Seq(conc))
    def apply(concs: Seq[TypingRuleJudgment]): TypingRule = TypingRule(name, Seq(), concs)
  }

  //support for typing rules with just one premise
  implicit class _TypingRulePartial(prem: TypingRuleJudgment) {
    def ===>(name: String): _Concmissing = _Concmissing(name)

    case class _Concmissing(name: String) {
      def apply(conc: TypingRuleJudgment): TypingRule = TypingRule(name, Seq(prem), Seq(conc))
      def apply(concs: Seq[TypingRuleJudgment]): TypingRule = TypingRule(name, Seq(prem), concs)
    }
  }


  implicit class _TypingRulePartialFET(prem: FunExpTree) {
    def ===>(name: String): _Concmissing = _Concmissing(name)

    case class _Concmissing(name: String) {
      def apply(conc: TypingRuleJudgment): TypingRule = TypingRule(name, Seq(_toFunctionExpJudgment(prem)), Seq(conc))
      def apply(concs: Seq[TypingRuleJudgment]): TypingRule = TypingRule(name, Seq(_toFunctionExpJudgment(prem)), concs)
    }
  }

  implicit class _TypingRulePartialST(prem: SymTree) {

    //this is not so nice - what it does is:
    // 1) convert prem SymTree to a FunExpMetaTree first (so that it can contain meta variables!
    // 2) catch the corner case where the entire premise is a meta variable by itself, which is forbidden
    // 3) otherwise, cast into a FunExpTree, which is what we require to build a FuntionExpJudgment
    val premFunExpTree : FunExpTree = _symTreeToFunExpMetaTree(prem) match {
      case mv@MVarNode(_) => sys.error("Meta variables cannot appear as individual premise: " + mv)
      case fet : FunExpTree => fet
      case _ => sys.error("When trying to create a TypingRule premise, encountered an unsupported function construct")
    }

    def ===>(name: String): _Concmissing = _Concmissing(name)

    case class _Concmissing(name: String) {
      def apply(conc: TypingRuleJudgment): TypingRule = TypingRule(name, Seq(_toFunctionExpJudgment(premFunExpTree)), Seq(conc))
      def apply(concs: Seq[TypingRuleJudgment]): TypingRule = TypingRule(name, Seq(_toFunctionExpJudgment(premFunExpTree)), concs)
    }
  }

  //support for typing rules with multiple premises
  implicit class _TypingRulePartialSeq(prems: Seq[TypingRuleJudgment]) {
    def ===>(name: String): _Concmissing = _Concmissing(name)

    case class _Concmissing(name: String) {
      def apply(conc: TypingRuleJudgment): TypingRule = TypingRule(name, prems, Seq(conc))
      def apply(concs: Seq[TypingRuleJudgment]): TypingRule = TypingRule(name, prems, concs)
    }
  }

  //Careful: in the context of typing rules, you have to manually ensure that SymTree's are always converted to FunExpMetaTree's
  // and not to FunExpTree (which the implicit conversion would do!)

  implicit class _PartialTypingJudgmentSimpleSym(f1: Symbol) {
    //careful: method name ending with colon turns the order around!
    def :: (f2: MVarNode) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f2), _funExpMetaTreeToFunExpMeta(f1))
    def :: (f2: FunExpMetaTree) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f2), _funExpMetaTreeToFunExpMeta(f1))
    def :: (f2: SymTree) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(_symTreeToFunExpMetaTree(f2)), _funExpMetaTreeToFunExpMeta(f1))
    def :: (f2: Symbol) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f2), _funExpMetaTreeToFunExpMeta(f1))
  }

  implicit class _PartialTypingJudgmentSimpleMV(f1: MVarNode) {
    //careful: method name ending with colon turns the order around!
    def :: (f2: MVarNode) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f2), _funExpMetaTreeToFunExpMeta(f1))
    def :: (f2: FunExpMetaTree) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f2), _funExpMetaTreeToFunExpMeta(f1))
    def :: (f2: SymTree) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(_symTreeToFunExpMetaTree(f2)), _funExpMetaTreeToFunExpMeta(f1))
    def :: (f2: Symbol) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f2), _funExpMetaTreeToFunExpMeta(f1))
  }

  implicit class _PartialTypingJudgmentSimpleST(f1: SymTree) {
    //careful: method name ending with colon turns the order around!
    def :: (f2: MVarNode) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f2), _funExpMetaTreeToFunExpMeta(_symTreeToFunExpMetaTree(f1)))
    def :: (f2: FunExpMetaTree) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f2), _funExpMetaTreeToFunExpMeta(_symTreeToFunExpMetaTree(f1)))
    def :: (f2: SymTree) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(_symTreeToFunExpMetaTree(f2)), _funExpMetaTreeToFunExpMeta(_symTreeToFunExpMetaTree(f1)))
    def :: (f2: Symbol) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f2), _funExpMetaTreeToFunExpMeta(_symTreeToFunExpMetaTree(f1)))
  }

  implicit class _PartialTypingJudgmentSimpleFMT(f1: FunExpMetaTree) {
    //careful: method name ending with colon turns the order around!
    def :: (f2: MVarNode) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f2), _funExpMetaTreeToFunExpMeta(f1))
    def :: (f2: FunExpMetaTree) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f2), _funExpMetaTreeToFunExpMeta(f1))
    def :: (f2: SymTree) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(_symTreeToFunExpMetaTree(f2)), _funExpMetaTreeToFunExpMeta(f1))
    def :: (f2: Symbol) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f2), _funExpMetaTreeToFunExpMeta(f1))
  }

  implicit class _PartialTypingJudgmentSym(f1: Symbol) {
    def |- (ts: TypingJudgmentSimple) = TypingJudgment(_funExpMetaTreeToFunExpMeta(f1), ts.f1, ts.f2)
  }

  //need to have this specific class to deal with MVarNode, who are both SymTree and FunExpMetaTree
  implicit class _PartialTypingJudgmentMV(f1: MVarNode) {
    def |- (ts: TypingJudgmentSimple) = TypingJudgment(_funExpMetaTreeToFunExpMeta(f1), ts.f1, ts.f2)
  }

  implicit class _PartialTypingJudgmentST(f1: SymTree) {
    def |- (ts: TypingJudgmentSimple) = TypingJudgment(_funExpMetaTreeToFunExpMeta(_symTreeToFunExpMetaTree(f1)), ts.f1, ts.f2)
  }

  implicit class _PartialTypingJudgmentFMT(f1: FunExpMetaTree) {
    def |- (ts: TypingJudgmentSimple) = TypingJudgment(_funExpMetaTreeToFunExpMeta(f1), ts.f1, ts.f2)
  }

  /* //This would be needed if complex function expressions shall be supported as arguments of typing judgments
  // however, it would not directly work: there would be ambiguities with implicit class _PartialTypingJudgmentST(f1: SymTree)
  implicit class _PartialTypingJudgment(f1: FunExpMetaTree) {
    def :: (f2: FunExpMetaTree) = TypingJudgmentSimple(_funExpMetaTreeToFunExpMeta(f1), _funExpMetaTreeToFunExpMeta(f2))
    def |- (ts: TypingJudgmentSimple) = TypingJudgment(_funExpMetaTreeToFunExpMeta(f1), ts.f1, ts.f2)
  }*/

  implicit def _toFunctionExpJudgment(f: FunctionExp): FunctionExpJudgment = FunctionExpJudgment(f)

  implicit def _toFunctionExpJudgment(f: SymTree): FunctionExpJudgment = _toFunctionExpJudgment(_symTreeToFunExpMetaTree(f))

  implicit def _toFunctionExpJudgment(f: FunExpMetaTree): FunctionExpJudgment =
    f match {
      case MVarNode(_) => sys.error("cannot have a single meta variable as a FunctionExpJudgment: " + f)
      case fexp : FunExpTree => FunctionExpJudgment(_funExpTreeToFunExp(fexp))
      case _ => sys.error("encountered unsupported language construct when trying to create a FunctionExpJudgment")
    }

  def exists(mvs: MVarNode*): _existsJudgmentPartial = _existsJudgmentPartial(mvs map {_.mv})

  case class _existsJudgmentPartial(mvs: Seq[MetaVar]) {
    def |(jdg: TypingRuleJudgment) = ExistsJudgment(mvs, Seq(jdg))
    def |(jdglist: Seq[TypingRuleJudgment]) = ExistsJudgment(mvs, jdglist)
  }

  def forall(mvs: MVarNode*): _forallJudgmentPartial = _forallJudgmentPartial(mvs map {_.mv})

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
    def &(next: FunExpMetaTree): Seq[TypingRuleJudgment] =
      next match {
        case MVarNode(_) => sys.error("found an expression that contains a meta variable at a place where this is not possible (e.g. function definition)")
        case re : FunExpTree => Seq(jdg) :+ _toFunctionExpJudgment(re)
        case _ => sys.error("When trying to create an AndNode, encountered an unsupported function construct")
      }
  }

  implicit class _toTypingRuleJudgmentSeqSingleST(jdg: SymTree) {
    def &(next: TypingRuleJudgment): Seq[TypingRuleJudgment] = Seq(_toFunctionExpJudgment(jdg)) :+ next
    def &(next: FunExpTree): Seq[TypingRuleJudgment] = Seq(_toFunctionExpJudgment(jdg)) :+ _toFunctionExpJudgment(next)
    def &(next: FunExpMetaTree): Seq[TypingRuleJudgment] =
      next match {
        case MVarNode(_) => sys.error("found an expression that contains a meta variable at a place where this is not possible (e.g. function definition)")
        case re : FunExpTree => Seq(_toFunctionExpJudgment(jdg)) :+ _toFunctionExpJudgment(re)
        case _ => sys.error("When trying to create an AndNode, encountered an unsupported function construct")
      }
  }

  implicit class _toTypingRuleJudgmentSeqSingleFET(jdg: FunExpTree) {
    def &(next: TypingRuleJudgment): Seq[TypingRuleJudgment] = Seq(_toFunctionExpJudgment(jdg)) :+ next
    def &(next: FunExpTree): Seq[TypingRuleJudgment] = Seq(_toFunctionExpJudgment(jdg)) :+ _toFunctionExpJudgment(next)
    def &(next: FunExpMetaTree): Seq[TypingRuleJudgment] =
      next match {
        case MVarNode(_) => sys.error("found an expression that contains a meta variable at a place where this is not possible (e.g. function definition)")
        case re : FunExpTree => Seq(_toFunctionExpJudgment(jdg)) :+ _toFunctionExpJudgment(re)
        case _ => sys.error("When trying to create an AndNode, encountered an unsupported function construct")
      }
  }

  implicit class _toTypingRuleJudgmentSeq(jdgs: Seq[TypingRuleJudgment]) {
    def &(next: TypingRuleJudgment): Seq[TypingRuleJudgment] = jdgs :+ next
    def &(next: FunExpTree): Seq[TypingRuleJudgment] = jdgs :+ _toFunctionExpJudgment(next)
    def &(next: FunExpMetaTree): Seq[TypingRuleJudgment] =
      next match {
        case MVarNode(_) => sys.error("found an expression that contains a meta variable at a place where this is not possible (e.g. function definition)")
        case re : FunExpTree => jdgs :+ _toFunctionExpJudgment(re)
        case _ => sys.error("When trying to create an AndNode, encountered an unsupported function construct")
      }
  }

  def OR(orcases: Seq[Seq[TypingRuleJudgment]]) = OrJudgment(orcases)

  def =>> (tjdg: TypingRuleJudgment) = Seq(Seq(tjdg))
  def =>> (tjdgs: Seq[TypingRuleJudgment]) = Seq(tjdgs)

  implicit class _appendOrcase(orcases: Seq[Seq[TypingRuleJudgment]]) {
    def |(next: Seq[Seq[TypingRuleJudgment]]): Seq[Seq[TypingRuleJudgment]] = orcases ++ next
  }
}
