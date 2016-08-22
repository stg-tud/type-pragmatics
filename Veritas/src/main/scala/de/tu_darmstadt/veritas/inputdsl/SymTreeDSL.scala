package de.tu_darmstadt.veritas.inputdsl

/**
  * Common DSL syntax for writing lists of symbols, in next notation
  */
object SymTreeDSL {

  sealed abstract class SymTree

  case class SymLeaf(s: Symbol) extends SymTree
  case class SymNode(s: Symbol, childlist: Seq[SymTree]) extends SymTree

  implicit def _toSymLeaf(s: Symbol): SymLeaf = SymLeaf(s)

  implicit def _toSymNode(s: Symbol)(symargs: Seq[SymTree]) = SymNode(s, symargs)

  implicit def _toSymLeafSeq(s: Symbol): Seq[SymLeaf] = Seq(s)

  implicit def _toSymTreeSeq(s: Symbol): _SymTreeSeq = new _SymTreeSeq(s)

  implicit def _toSymTreeSeq(s: SymLeaf): _SymTreeSeq = new _SymTreeSeq(Seq(s))

  implicit def _toSymTreeSeq(s: SymNode): _SymTreeSeq = new _SymTreeSeq(Seq(s))

  // currently supports "-" and "|" for appending symbols to the list
  implicit class _SymTreeSeq(elems: Seq[SymTree]) {
    def -(next: SymTree): Seq[SymTree] = elems :+ next
    def |(next: SymTree): Seq[SymTree] = elems :+ next
  }

}
