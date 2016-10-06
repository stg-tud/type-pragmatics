package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast.{Functions, MetaVar, PartialFunctions, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.inputdsl.FunctionDSL.FunExpTrue
import de.tu_darmstadt.veritas.inputdsl.SymTreeDSL._
/**
  * DSL for top-level function definition syntax
  */
object FunctionDSL {

  def partial(funcs: Functions) = funcs match {
    case Functions(sfd) => PartialFunctions(sfd)
  }

  // top-level syntax for creating a function definition
  def function(sig: FunctionSig, eqs: Seq[FunctionEq] = Seq()) = Functions(Seq(FunctionDef(sig, eqs)))

  // starting point for generating a function signature
  implicit class _FunctionSigPartial(name: Symbol) {

    class _FunctionSigWithNameArgs(args: Seq[Symbol]) {
      def ->(ressym: Symbol) = new FunctionSig(name.name, args map { s => SortRef(s.name) }, SortRef(ressym.name))
    }

    def >>(args: Symbol*) = new _FunctionSigWithNameArgs(args.toSeq)

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

    private def _funExpTreeToFunExp(exptree: FunExpMetaTree): FunctionExp = exptree match {
      case VarLeaf(s) => FunctionExpVar(s.name)
      case AppNode(s, children) => FunctionExpApp(s.name, children map {_funExpTreeToFunExp(_)})
    }

    def :=(exptree: FunExpTree) = FunctionEq(fn, functionPats, _funExpTreeToFunExp(exptree))

    //required extra support for function equations with just one symbol
    def :=(s: Symbol) = FunctionEq(fn, functionPats, FunctionExpVar(s.name))

  }

  implicit class MVSymbol(s: Symbol) {
    def unary_~ : MetaVar = MetaVar(s.name)
  }

  //required extra support for function expressions starting with just a symbol
  implicit def _symToFunExpMetaTree(s: Symbol): FunExpMetaTree = VarLeaf(s)

  abstract class FunExpMetaTree {
    def ===(rexp: FunExpMetaTree) = EqNode(this, rexp)
    def ~=(rexp: FunExpMetaTree) = NeqNode(this, rexp)
  }

  case class MVarNode(mv: MetaVar) extends FunExpMetaTree

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
  //case class IfNode(guard: FunExpTree, th: FunExpMetaTree, els: FunExpMetaTree) extends FunExpTree
  //case class LetNode(s: Symbol, named: FunExpMetaTree, in: FunExpMetaTree) extends FunExpTree

  implicit def _symTreeToFunExpTree(st: SymTree): FunExpTree = st match {
    case SymLeaf(sn) => VarLeaf(sn)
    case SymNode(sn, childlist) => AppNode(sn, childlist map { (stc : SymTree) => _symTreeToFunExpTree(stc)})
  }

  implicit def _boolToFunExp(b: Boolean): FunExpTree = if (b) FunExpTrue else FunExpFalse


}
