package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast.{DataType, DataTypeConstructor, SortRef}

/**
  * DSL for data type definitions
  */
object DataTypeDSL {

  import SortRefDSL._

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

  // support for creating a single data type constructor from a symbol
  implicit def _toConstr(s: Symbol): DataTypeConstructor = DataTypeConstructor(s.name, Seq())

  implicit def _toConstrL(s: Symbol): Seq[DataTypeConstructor] = Seq(DataTypeConstructor(s.name, Seq()))

  implicit def _toConstrArgs(s: Symbol)(args: Seq[SortRef]) : DataTypeConstructor =
    DataTypeConstructor(s.name, args)

  // create a list of data type constructors - end point
  implicit def _toConstrList(s: Symbol) = new _ConstrList(Seq(s))

  // create a list of data type constructors - end point (from DataTypeConstructor)
  implicit def _toConstrList(c: DataTypeConstructor): _ConstrList = new _ConstrList(Seq(c))

  // create a list of data type constructors where new constructors can be added via | syntax
  implicit class _ConstrList(cons: Seq[DataTypeConstructor]) {
    def |(next: DataTypeConstructor): Seq[DataTypeConstructor] = cons :+ next
  }

}