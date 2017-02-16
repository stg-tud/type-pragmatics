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

    // when receiving trees, we have to check that there are not meta variables
    def :=(exptree: FunExpTree) =
      if (funExpMetaTreeContainsMV(exptree))
        sys.error("found an expression that contains a meta variable at a place where this is not possible (e.g. function definition)")
      else FunctionEq(fn, functionPats, _funExpTreeToFunExp(exptree))

    // can even receive a FunExpMetaTree (might sometimes happen, if a tree is generated from a SymTree expression
    // but then, we have to downcast (checking first that there are no meta variables!
    def :=(exptree: FunExpMetaTree) =
    if (funExpMetaTreeContainsMV(exptree))
      sys.error("found an expression that contains a meta variable at a place where this is not possible (e.g. function definition)")
    else FunctionEq(fn, functionPats, _funExpTreeToFunExp(exptree.asInstanceOf[FunExpTree])) //since we check for meta variables before, the cast should never fail...

    //required extra support for function equations with just one symbol
    def :=(s: Symbol) = FunctionEq(fn, functionPats, FunctionExpVar(s.name))

  }

  //required extra support for function expressions starting with just a symbol
  implicit def _symToFunExpMetaTree(s: Symbol): FunExpMetaTree = VarLeaf(s)

  abstract class FunExpMetaTree {
    def ===(rexp: FunExpMetaTree) = EqNode(this, rexp)

    def ~=(rexp: FunExpMetaTree) = NeqNode(this, rexp)

    def unary_! : FunExpTree = this match {
      case MVarNode(_) => sys.error("found an expression that contains a meta variable at a place where this is not possible (e.g. function definition)")
      case re : FunExpTree => !re
      case _ => sys.error("When trying to create an AndNode, encountered an unsupported function construct")
    }

    //also allow to call &&, ||, <=> on FunExpMetaTree, but will fail at runtime if top-level construct is a meta variable
    def &&(rexp: FunExpMetaTree): FunExpTree =
      this match {
        case MVarNode(_) => sys.error("found an expression that contains a meta variable at a place where this is not possible (e.g. function definition)")
        case re : FunExpTree => re.&&(rexp)
        case _ => sys.error("When trying to create an AndNode, encountered an unsupported function construct")
      }

    def ||(rexp: FunExpMetaTree): FunExpTree =
      this match {
        case MVarNode(_) => sys.error("found an expression that contains a meta variable at a place where this is not possible (e.g. function definition)")
        case re : FunExpTree => re.||(rexp)
        case _ => sys.error("When trying to create an AndNode, encountered an unsupported function construct")
      }


    def <=>(rexp: FunExpMetaTree): FunExpTree =
      this match {
        case MVarNode(_) => sys.error("found an expression that contains a meta variable at a place where this is not possible (e.g. function definition)")
        case re : FunExpTree => re.<=>(rexp)
        case _ => sys.error("When trying to create an AndNode, encountered an unsupported function construct")
      }

  }

  case class MVarNode(mv: MetaVar) extends FunExpMetaTree with SymTree

  def funExpMetaTreeContainsMV(fmt: FunExpMetaTree): Boolean =
    fmt match {
      case MVarNode(_) => true
      case FunExpTrue => false
      case FunExpFalse => false
      case VarLeaf(s) => false
      case AppNode(s, childlist) => childlist exists (c => funExpMetaTreeContainsMV(c))
      case NotNode(child) => funExpMetaTreeContainsMV(child)
      case EqNode(left, right) => funExpMetaTreeContainsMV(left) || funExpMetaTreeContainsMV(right)
      case NeqNode(left, right) => funExpMetaTreeContainsMV(left) || funExpMetaTreeContainsMV(right)
      case AndNode(left, right) => funExpMetaTreeContainsMV(left) || funExpMetaTreeContainsMV(right)
      case OrNode(left, right) => funExpMetaTreeContainsMV(left) || funExpMetaTreeContainsMV(right)
      case BiImplNode(left, right) => funExpMetaTreeContainsMV(left) || funExpMetaTreeContainsMV(right)
      case IfNode(guard, th, els) => funExpMetaTreeContainsMV(guard) || funExpMetaTreeContainsMV(th) || funExpMetaTreeContainsMV(els)
      case LetNode(s, named, in) => funExpMetaTreeContainsMV(named) || funExpMetaTreeContainsMV(in)
      case _ => sys.error("Encountered an unexpected construct when checking whether a given function expressions contains a meta variable or not.")
    }

  implicit class MVSymbol(s: Symbol) {
    def unary_~ : MVarNode = MVarNode(MetaVar(s.name))
  }

  abstract class FunExpTree extends FunExpMetaTree {
    override def unary_! : FunExpTree = NotNode(this)

    //let all the binary operations also accept FunExpMetaTree expressions (might be required since SymTrees are converted to FunExpMetaTree
    //but check that they don't contain meta variables top level, since this is forbidden
    override def &&(rexp: FunExpMetaTree): FunExpTree =
      rexp match {
        case MVarNode(_) => sys.error("found an expression that contains a meta variable at a place where this is not possible (e.g. function definition)")
        case re : FunExpTree => AndNode(this, re)
        case _ => sys.error("When trying to create an AndNode, encountered an unsupported function construct")
      }

    override def ||(rexp: FunExpMetaTree): FunExpTree =
      rexp match {
        case MVarNode(_) => sys.error("found an expression that contains a meta variable at a place where this is not possible (e.g. function definition)")
        case re : FunExpTree => OrNode(this, re)
        case _ => sys.error("When trying to create an OrNode, encountered an unsupported function construct")
      }

    override def <=>(rexp: FunExpMetaTree): FunExpTree =
      rexp match {
        case MVarNode(_) => sys.error("found an expression that contains a meta variable at a place where this is not possible (e.g. function definition)")
        case re : FunExpTree => BiImplNode(this, re)
        case _ => sys.error("When trying to create a BiImplNode, encountered an unsupported function construct")
      }
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

  //  implicit def _symTreeToFunExpTree(st: SymTree): FunExpTree = st match {
  //    case SymLeaf(sn) => VarLeaf(sn)
  //    case SymNode(sn, childlist) => AppNode(sn, childlist map { (stc: SymTree) => _symTreeToFunExpTree(stc) })
  //    case MVarNode(mv) => sys.error(s"Cannot accept a meta variable $mv here! (happens for example if you try to use a meta variable in a function definition)")
  //  }

  //this is deliberately not marked as implicit, since otherwise it would clash with _symTreeToFunExpTree from above
  //in function definitions, we want that the top-level SymTrees are always converted to FunExpTree (no meta variables!)
  //however, in typing rules, SymTrees may contain meta variables - and then, _symTreeToFunExpMetaTree should be called
  //explicitly to ensure that the correct type is returned
  implicit def _symTreeToFunExpMetaTree(st: SymTree): FunExpMetaTree = st match {
    case SymLeaf(sn) => VarLeaf(sn)
    case SymNode(sn, childlist) => AppNode(sn, childlist map { (stc: SymTree) => _symTreeToFunExpMetaTree(stc) })
    case mv@MVarNode(_) => mv
  }

  implicit def _boolToFunExp(b: Boolean): FunExpTree = if (b) FunExpTrue else FunExpFalse

  def iff(gexp: FunExpMetaTree) = gexp match {
    case MVarNode(_) => sys.error("Guards of iffs cannot contain a meta variable top level.")
    case gexp : FunExpTree => _PartialIf1(gexp)
    case _ => sys.error("Encountered an unsupported function construct when creating an if-guard.")
  }

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
