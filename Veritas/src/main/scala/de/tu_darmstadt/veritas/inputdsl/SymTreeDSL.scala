package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpMeta

/**
  * Common DSL syntax for writing flat symbol trees
  * used for generating constructors of data types, function patterns, function expressions
  */

/*object SymTreeDSL {

  sealed abstract class SymTree

  case class SymLeaf(s: Symbol) extends SymTree
  case class SymNode(s: Symbol, childlist: Seq[SymTree]) extends SymTree

  implicit def _toSymLeaf(s: Symbol): SymLeaf = SymLeaf(s)

  implicit class _toSymTree(sym: Symbol) {
    def apply(argsym: SymTree*) = SymNode(sym, argsym)
  }
}*/

object ASTTreeDSL {

  abstract class SymTree

  case class SymLeaf(s: Symbol) extends SymTree
  case class SymNode(s: Symbol, childlist: Seq[SymTree]) extends SymTree

  //sealed abstract class SymTree extends ASTTree

  implicit def _toSymLeaf(s: Symbol): SymLeaf = SymLeaf(s)

  implicit class _toSymTree(sym: Symbol) {
    def apply(argsym: SymTree*) = SymNode(sym, argsym)
  }

}

