package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast.{Functions, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.inputdsl.SymTreeDSL._

/**
  * DSL for top-level function definition syntax
  */
object FunctionDSL {

  //import SortRefDSL._

  // top-level syntax for creating a function definition
  def function(sig: FunctionSig, eqs: Seq[FunctionEq] = Seq()) = Functions(Seq(FunctionDef(sig, eqs)))

  // starting point for generating a function signature
  implicit class _FunctionSigPartial(name: Symbol) {
    // for function signatures with just one input argument
    def >>(params: (Symbol, Symbol)): FunctionSig =
    new FunctionSig(name.name, Seq(SortRef(params._1.name)), SortRef(params._2.name))

    // for function signatures with several input arguments
    def >>(params: (Seq[SymTree], Symbol)): FunctionSig =
    new FunctionSig(name.name, params._1 map { case SymLeaf(sn) => SortRef(sn.name) }, SortRef(params._2.name))
  }

  // adding the right-hand-side to a function equation
  implicit class _FunctionEqPartialST(st: SymTree) {
    private def symTreeToFunctionPatSeq(st: SymTree): FunctionPattern = st match {
      case SymLeaf(s) => FunctionPatVar(s.name)
      case SymNode(s, children) => FunctionPatApp(s.name, children map {symTreeToFunctionPatSeq(_)})
    }

    private val fn: String = st match {
      case SymNode(s, _) => s.name
    }

    private val functionPats: Seq[FunctionPattern] = st match {
      case SymNode(_, ch) => ch map {symTreeToFunctionPatSeq(_)}
      case SymLeaf(_) => Seq() //should not happen
    }


    def :=(exp: FunctionExp): FunctionEq = FunctionEq(fn, functionPats, exp)
  }


    //TODO: this is like constructor lists in DataTypeDSL - maybe abstract this further?

  // create a list of function equations - end point
  implicit def _toFunEqList(feq: FunctionEq) = new _FunEqList(Seq(feq))

  // create a list of function eqs where new equations can be added via | syntax
  implicit class _FunEqList(eqs: Seq[FunctionEq]) {
    def |(next: FunctionEq): Seq[FunctionEq] = eqs :+ next
  }

}
