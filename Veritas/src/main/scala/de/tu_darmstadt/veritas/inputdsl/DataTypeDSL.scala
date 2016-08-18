package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast.{DataType, DataTypeConstructor, SortRef}

object DataTypeDSL {

  implicit def _toConstr(s: Symbol): DataTypeConstructor = DataTypeConstructor(s.name, Seq())

  implicit class _ConstrList(cons: Seq[DataTypeConstructor]) {
    def |(next: DataTypeConstructor): Seq[DataTypeConstructor] = cons :+ next
  }
  implicit def _toConstrList(s: Symbol) = new _ConstrList(Seq(s))

  class _toC(c: DataTypeConstructor) {
    def > :DataTypeConstructor = c
  }

  implicit def _toSortRef(s: Symbol): SortRef = SortRef(s.name)

  class _ParamList(name: Symbol) {
    def < (params: Symbol*): _toC = new _toC(DataTypeConstructor(name.name, params map {_toSortRef(_)} ))
  }

  implicit def _toParamList(s: Symbol): _ParamList = new _ParamList(s)
  implicit def _toConstrList(c: DataTypeConstructor): _ConstrList = new _ConstrList(Seq(c))


  class _DataTypePartial(val open: Boolean, val name: Symbol) {
    def of(constr: Seq[DataTypeConstructor]): DataType = {
      DataType(open, name.name, constr)
    }
  }

  class _OpenDataTypePartial {
    def data(name: Symbol) = new _DataTypePartial(true, name)
  }

  implicit def _emptyToDataType(dt: _DataTypePartial): DataType =
    DataType(dt.open, dt.name.name, Seq())

  def open = new _OpenDataTypePartial()

  def data(name: Symbol) = new _DataTypePartial(false, name)
}