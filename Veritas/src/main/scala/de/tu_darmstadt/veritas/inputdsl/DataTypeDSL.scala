package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast.{DataType, DataTypeConstructor, SortRef}
import de.tu_darmstadt.veritas.inputdsl.SymTreeDSL.{SymLeaf, SymNode, SymTree}

/**
  * DSL for data type definitions
  */
object DataTypeDSL {

  // concrete syntax - open
  def open = new _OpenDataTypePartial()

  // concrete syntax - closed
  def data(name: Symbol) = new _DataTypePartial(false, name)

  // supporting intermediate class - data type, being able to await list of constructors
  class _DataTypePartial(val open: Boolean, val name: Symbol) {
    def of(constr: DataTypeConstructor): DataType = {
      DataType(open, name.name, Seq(constr))
    }

    def of(constr: Seq[DataTypeConstructor]): DataType = {
      DataType(open, name.name, constr)
    }

    // support for single, non-argument constructor
    def of(cons: Symbol): DataType = {
      DataType(open, name.name, Seq(_toDataTypeConstructor(cons)))
    }
  }

  // supporting intermediate class for open data types
  class _OpenDataTypePartial {
    def data(name: Symbol) = new _DataTypePartial(true, name)
  }

  // support for empty data types
  implicit def _emptyToDataType(dt: _DataTypePartial): DataType =
    DataType(dt.open, dt.name.name, Seq())

  //construct DataTypeConstructor from SymTree
  implicit def _toDataTypeConstructor(s: SymTree): DataTypeConstructor = {
    s match {
      case SymLeaf(s) => DataTypeConstructor(s.name, Seq())
      case SymNode(s, children) => {
        // this mapping will fail if the child list does not consist of SymLeafs only!
        // currently, this is intentional behavior, it shall not be possible to construct
        // DataTypeConstructors where the child list consists of SymNodes
        val arglist = children map { case SymLeaf(sn) => SortRef(sn.name) }
        DataTypeConstructor(s.name, arglist)
      }
    }
  }

  implicit class _ConstrListSingleSym(s: Symbol) {
    def |(next: SymTree): Seq[DataTypeConstructor] = Seq(_toDataTypeConstructor(s)) :+ _toDataTypeConstructor(next)
  }

  implicit class _ConstrListSingleST(st: SymTree) {
    def |(next: SymTree): Seq[DataTypeConstructor] = Seq(_toDataTypeConstructor(st)) :+ _toDataTypeConstructor(next)
  }

  implicit class _ConstrListSingle(dc: DataTypeConstructor) {
    def |(next: SymTree): Seq[DataTypeConstructor] = Seq(dc) :+ _toDataTypeConstructor(next)
  }

  implicit class _ConstrList(st: Seq[DataTypeConstructor]) {
    def |(next: SymTree): Seq[DataTypeConstructor] = st :+ _toDataTypeConstructor(next)
  }

}