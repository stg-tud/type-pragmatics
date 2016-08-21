package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast.{Functions, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function._

/**
  * DSL for top-level function definition syntax
  */
object FunctionDSL {
  import SortRefDSL._

  // top-level syntax for creating a function definition
  def function(sig: FunctionSig, eqs: Seq[FunctionEq] = Seq()) = Functions(Seq(FunctionDef(sig, eqs)))

  // starting point for generating a function signature
  implicit class _FunctionSigPartial(name: Symbol) {
    // for function signatures with just one input argument
    def >> (params: (Symbol, Symbol)): FunctionSig =
      new FunctionSig(name.name, Seq(params._1), params._2)

    // for function signatures with several input arguments
    def >> (params: (Seq[SortRef], Symbol)): FunctionSig =
      new FunctionSig(name.name, params._1, params._2)
  }

  // starting point for generating a single function equation
  implicit def _toFunctionEqPartial(fn: Symbol)(args: Seq[FunctionPattern]): _FunctionEqPartial = new _FunctionEqPartial(fn, args)

  // adding the right-hand-side to a function equation
  class _FunctionEqPartial(fn: Symbol, args: Seq[FunctionPattern]) {
    def == (exp: FunctionExp): FunctionEq = FunctionEq(fn.name, args, exp)
  }

  //TODO: this is like constructor lists in DataTypeDSL - maybe abstract this further?

  // create a list of function equations - end point
  implicit def _toFunEqList(feq: FunctionEq) = new _FunEqList(Seq(feq))

  // create a list of function eqs where new equations can be added via | syntax
  implicit class _FunEqList(eqs: Seq[FunctionEq]) {
    def |(next: FunctionEq): Seq[FunctionEq] = eqs :+ next
  }
}

//object FunctionPatDSL {
//
//  // conversion to FunctionPatVar
//  implicit def _toFunctionPatVar(v: Symbol): FunctionPatVar = FunctionPatVar(v.name)
//
//  //conversion to FunctionPatApplication
//  implicit def _toFunctionPatApp(v: Symbol)(args: Seq[FunctionPattern]): FunctionPatApp = FunctionPatApp(v.name, args)
//
//  implicit def _toFunctionPatList(fp: FunctionPattern): Seq[FunctionPattern] = Seq(fp)
//
//  // create a list of function patterns - end point
//  implicit def _toFPList(s: Symbol) = new _FPList(Seq(s))
//
//  // create a list of function patterns - end point (from FunctionPattern)
//  implicit def _toFPList(fp: FunctionPattern): _FPList = new _FPList(Seq(fp))
//
//  // create a list of sort references where new sort refs can be added via - syntax
//  implicit class _FPList(fp: Seq[FunctionPattern]) {
//    def +(next: FunctionPattern): Seq[FunctionPattern] = fp :+ next
//  }
//
//}
