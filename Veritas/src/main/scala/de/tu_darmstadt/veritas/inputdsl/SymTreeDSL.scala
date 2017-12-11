package de.tu_darmstadt.veritas.inputdsl

import scala.language.implicitConversions
/**
  * Common DSL syntax for writing flat symbol trees
  * used for generating constructors of data types, function patterns, function expressions
  */

object SymTreeDSL {

  trait SymTree

  case class SymLeaf(s: Symbol) extends SymTree
  case class SymNode(s: Symbol, childlist: Seq[SymTree]) extends SymTree

  implicit def _toSymLeaf(s: Symbol): SymLeaf = SymLeaf(s)

  implicit class _toSymTree(sym: Symbol) {
    def apply(argsym: SymTree*) = SymNode(sym, argsym)
  }

}

