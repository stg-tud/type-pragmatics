package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast.{Functions, MetaVar, PartialFunctions, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.inputdsl.SymTreeDSL._

/**
  * DSL for top-level function definition syntax
  */
object FunctionDSL {

  def partial(funcs: Functions) = funcs match {
    case Functions(sfd) => PartialFunctions(sfd)
  }

  // top-level syntax for creating a function definition
  // first generates a function without any equations - keyword where may add equations (see _FunctionAwaitingEqs)
  def function(sig: FunctionSig) = Functions(Seq(FunctionDef(sig, Seq())))


  // starting point for generating a function signature
  implicit class _FunctionSigPartial(name: Symbol) {

    class _FunctionSigWithNameArgs(args: Seq[Symbol]) {
      def ->(ressym: Symbol) = new FunctionSig(name.name, args map { s => SortRef(s.name) }, SortRef(ressym.name))
    }

    def >>(args: Symbol*) = new _FunctionSigWithNameArgs(args.toSeq)

  }

  implicit class _FunctionAwaitingEqs(fun: Functions) {
    def where(eq: FunctionEq) = fun match {
      case Functions(Seq(FunctionDef(sig, preveq))) => Functions(Seq(FunctionDef(sig, preveq :+ eq)))
    }

    def where(eqs: Seq[FunctionEq]) = fun match {
      case Functions(Seq(FunctionDef(sig, preveq))) => Functions(Seq(FunctionDef(sig, preveq ++ eqs)))
    }
  }

  implicit class _FunctionEqListSingle(fe: FunctionEq) {
    def |(next: FunctionEq): Seq[FunctionEq] = Seq(fe) :+ next
  }

  implicit class _FunctionEqList(fe: Seq[FunctionEq]) {
    def |(next: FunctionEq): Seq[FunctionEq] = fe :+ next
  }


  // adding the right-hand-side to a function equation
  implicit class _FunctionEqPartialST(st: SymTree) {
    private def symTreeToFunctionPatSeq(st: SymTree): FunctionPattern = st match {
      case SymLeaf(s) => FunctionPatVar(s.name)
      case SymNode(s, children) => FunctionPatApp(s.name, children map {
        symTreeToFunctionPatSeq(_)
      })
    }

    private val fn: String = st match {
      case SymNode(s, _) => s.name
    }

    private val functionPats: Seq[FunctionPattern] = st match {
      case SymNode(_, ch) => ch map {
        symTreeToFunctionPatSeq(_)
      }
      case SymLeaf(_) => Seq() //should not happen
    }


    def :=(exp: FunctionExp) = FunctionEq(fn, functionPats, exp)

    def :=(exptree: FunExpTree) = FunctionEq(fn, functionPats, _funExpTreeToFunExp(exptree))

    //required extra support for function equations with just one symbol
    def :=(s: Symbol) = FunctionEq(fn, functionPats, FunctionExpVar(s.name))

  }

  //required extra support for function expressions starting with just a symbol
  implicit def _symToFunExpMetaTree(s: Symbol): FunExpMetaTree = VarLeaf(s)

  abstract class FunExpMetaTree {
    def ===(rexp: FunExpMetaTree) = EqNode(this, rexp)

    def ~=(rexp: FunExpMetaTree) = NeqNode(this, rexp)
  }


  implicit class MVSymbol(s: Symbol) {
    def unary_~ : MVarNode = MVarNode(MetaVar(s.name))
  }

  case class MVarNode(mv: MetaVar) extends FunExpMetaTree with SymTree

  abstract class FunExpTree extends FunExpMetaTree {
    def unary_! = NotNode(this)

    def &&(rexp: FunExpTree) = AndNode(this, rexp)

    def ||(rexp: FunExpTree) = OrNode(this, rexp)

    def <=>(rexp: FunExpTree) = BiImplNode(this, rexp)
  }

  object FunExpTrue extends FunExpTree

  object FunExpFalse extends FunExpTree

  case class VarLeaf(s: Symbol) extends FunExpTree

  case class AppNode(s: Symbol, childlist: Seq[FunExpMetaTree]) extends FunExpTree

  case class NotNode(child: FunExpTree) extends FunExpTree

  case class EqNode(left: FunExpMetaTree, right: FunExpMetaTree) extends FunExpTree

  case class NeqNode(left: FunExpMetaTree, right: FunExpMetaTree) extends FunExpTree

  case class AndNode(left: FunExpTree, right: FunExpTree) extends FunExpTree

  case class OrNode(left: FunExpTree, right: FunExpTree) extends FunExpTree

  case class BiImplNode(left: FunExpTree, right: FunExpTree) extends FunExpTree

  case class IfNode(guard: FunExpTree, th: FunExpMetaTree, els: FunExpMetaTree) extends FunExpTree

  case class LetNode(s: Symbol, named: FunExpMetaTree, in: FunExpMetaTree) extends FunExpTree

  implicit def _symTreeToFunExpTree(st: SymTree): FunExpTree = st match {
    case SymLeaf(sn) => VarLeaf(sn)
    case SymNode(sn, childlist) => AppNode(sn, childlist map { (stc: SymTree) => _symTreeToFunExpTree(stc) })
  }

  implicit def _boolToFunExp(b: Boolean): FunExpTree = if (b) FunExpTrue else FunExpFalse

  def iff(gexp: FunExpTree) = _PartialIf1(gexp)

  case class _PartialIf1(gexp: FunExpTree) {
    def th(texp: FunExpMetaTree) = _PartialIf2(gexp, texp)

    case class _PartialIf2(gexp: FunExpTree, texp: FunExpMetaTree) {
      def els(elexp: FunExpMetaTree) = IfNode(gexp, texp, elexp)
    }

  }

  def let(s: Symbol) = _PartialLet1(s)

  case class _PartialLet1(s: Symbol) {
    def :=(bind: FunExpMetaTree) = _PartialLet2(s, bind)

    case class _PartialLet2(s: Symbol, bind: FunExpMetaTree) {
      def in(body: FunExpMetaTree) = LetNode(s, bind, body)
    }

  }


  def _funExpMetaTreeToFunExpMeta(mexptree: FunExpMetaTree): FunctionExpMeta = mexptree match {
    case MVarNode(mv) => FunctionMeta(mv)
    case e: FunExpTree => _funExpTreeToFunExp(e)
  }

  def _funExpTreeToFunExp(exptree: FunExpTree): FunctionExp = exptree match {
    case FunExpTrue => FunctionExpTrue
    case FunExpFalse => FunctionExpFalse
    case VarLeaf(s) => FunctionExpVar(s.name)
    case AppNode(s, children) => FunctionExpApp(s.name, children map {
      _funExpMetaTreeToFunExpMeta(_)
    })
    case NotNode(child) => FunctionExpNot(_funExpTreeToFunExp(child))
    case EqNode(left, right) => FunctionExpEq(_funExpMetaTreeToFunExpMeta(left), _funExpMetaTreeToFunExpMeta(right))
    case NeqNode(left, right) => FunctionExpNeq(_funExpMetaTreeToFunExpMeta(left), _funExpMetaTreeToFunExpMeta(right))
    case AndNode(left, right) => FunctionExpAnd(_funExpTreeToFunExp(left), _funExpTreeToFunExp(right))
    case OrNode(left, right) => FunctionExpOr(_funExpTreeToFunExp(left), _funExpTreeToFunExp(right))
    case BiImplNode(left, right) => FunctionExpBiImpl(_funExpTreeToFunExp(left), _funExpTreeToFunExp(right))
    case IfNode(guard, thenpart, elsepart) => FunctionExpIf(_funExpTreeToFunExp(guard), _funExpMetaTreeToFunExpMeta(thenpart), _funExpMetaTreeToFunExpMeta(elsepart))
    case LetNode(s, bind, body) => FunctionExpLet(s.name, _funExpMetaTreeToFunExpMeta(bind), _funExpMetaTreeToFunExpMeta(body))
  }


}
