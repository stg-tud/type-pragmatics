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
    def of(constr: Seq[DataTypeConstructor]): DataType = {
      DataType(open, name.name, constr)
    }
  }

  // supporting intermediate class for open data types
  class _OpenDataTypePartial {
    def data(name: Symbol) = new _DataTypePartial(true, name)
  }

  // support for empty data types
  implicit def _emptyToDataType(dt: _DataTypePartial): DataType =
    DataType(dt.open, dt.name.name, Seq())

  // support for single, non-argument constructor
  implicit def _toDataTypeConstructor(s: Symbol): Seq[DataTypeConstructor] = Seq(DataTypeConstructor(s.name, Seq()))

  implicit def _toDataTypeConstructor(s: SymTree): DataTypeConstructor = {
    s match {
      case SymLeaf(s) => DataTypeConstructor(s.name, Seq())
      case SymNode(s, children) => {
        // this mapping will fail if the child list does not consist of SymLeafs only!
        val arglist = children map { case SymLeaf(sn) => SortRef(sn.name) }
        DataTypeConstructor(s.name, arglist)
      }
    }
  }

  implicit def _toDataTypeConstructorSeq(s: SymTree): Seq[DataTypeConstructor] = {
    s match {
      case SymLeaf(s) => Seq(DataTypeConstructor(s.name, Seq()))
      case SymNode(s, children) => {
        val arglist = children map { case SymLeaf(sn) => SortRef(sn.name) }
        Seq(DataTypeConstructor(s.name, arglist))
      }
    }
  }

  implicit def _toDataTypeConstructorSeq(s: Seq[SymTree]): Seq[DataTypeConstructor] = {
    s map {_toDataTypeConstructor(_)}
  }

  implicit def _toDataTypeConstructorSeq(cons: DataTypeConstructor): _ConstrList = new _ConstrList(Seq(cons))

  // create a list of data type constructors where new constructors can be added via | syntax
  implicit class _ConstrList(cons: Seq[DataTypeConstructor]) {
    def |(next: DataTypeConstructor): Seq[DataTypeConstructor] = cons :+ next
  }

}