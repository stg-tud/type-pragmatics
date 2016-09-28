package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast.{Functions, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.inputdsl.SymTreeDSL._

/**
  * DSL for top-level function definition syntax
  */
object FunctionDSL {

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

  }

  implicit def _symtoFunctionExpSingle(s: Symbol): FunctionExp = FunctionExpVar(s.name)


  implicit def _symtoFunctionExp(st: SymTree): FunctionExp = st match {
    case SymLeaf(s) => FunctionExpVar(s.name)
    case SymNode(s, children) => FunctionExpApp(s.name, children map {
      _symtoFunctionExp(_)
    })
  }

  implicit class _FunctionExpPartial(exp: FunctionExp) {
    //close parsing of a function expression and return to caller
    //def >(): FunctionExp = exp

    def &&(expr: FunctionExp) = FunctionExpAnd(exp, expr)

    def ||(expr: FunctionExp) = FunctionExpOr(exp, expr)


  }


}
