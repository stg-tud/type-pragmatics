package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast.MetaVar

/**
  * Common DSL syntax for writing flat symbol trees
  * used for generating constructors of data types, function patterns, function expressions
  */

object SymTreeDSL {

  implicit class MVSymbol(s: Symbol) {
    def unary_~ : MetaVar = MetaVar(s.name)
  }

  abstract class SymTree

  case class SymLeaf(s: Symbol) extends SymTree
  //case class SymMVLeaf(mv: MetaVar) extends SymTree
  case class SymNode(s: Symbol, childlist: Seq[SymTree]) extends SymTree

  implicit def _toSymLeaf(s: Symbol): SymLeaf = SymLeaf(s)

  //implicit def _toSymMVLeaf(mv: MetaVar): SymMVLeaf = SymMVLeaf(mv)

  implicit class _toSymTree(sym: Symbol) {
    def apply(argsym: SymTree*) = SymNode(sym, argsym)
  }

}

