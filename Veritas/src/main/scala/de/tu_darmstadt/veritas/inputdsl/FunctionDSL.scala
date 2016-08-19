package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast.{Functions, SortRef}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionEq, FunctionSig}

/**
  * Created by sylvia on 19/08/16.
  */
object FunctionDSL {
  import SortRefDSL._

  def function(sig: FunctionSig, eqs: Seq[FunctionEq]) = Functions(Seq(FunctionDef(sig, eqs)))

  implicit class _FunctionSigPartial(name: Symbol) {
    def >> (params: (Symbol, Symbol)): FunctionSig =
      new FunctionSig(name.name, Seq(params._1), params._2)

    def >> (params: (Seq[SortRef], Symbol)): FunctionSig =
      new FunctionSig(name.name, params._1, params._2)
  }

}
