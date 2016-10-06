package de.tu_darmstadt.veritas.inputdsl.testexamples

import de.tu_darmstadt.veritas.backend.ast.DataType
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}

/**
  * Created by cygne on 06.10.16.
  */
object SQLDefs {

  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._

  // name of attributes and tables
  val Name = open data 'Name

  // list of attribute names
  val AttrL = data('AttrL) of
    'aempty | 'acons ('Name, 'AttrL)


  val append = function('append.>>('AttrL, 'AttrL) -> 'AttrL)
  ('append ('aempty, 'y) := 'y) |
    ('append ('acons ('a, 'al), 'y) := 'acons ('a, 'append ('al, 'y)))


  val FType = open data 'FType

  // type of a table (table schema) - list of CType
  val TType = data('TType) of
    'ttempty | 'ttcons ('Name, 'FType, 'TType) // type of a single column (pair Name, FType)

  // Value for a field (underspecified)
  val Val = open data 'Val

  // table row, list of field values (with at least one cell/field per construction!)
  val Row = data('Row) of
    'rempty | 'rcons ('Val, 'Row)

  // table matrix (list of rows), without "header" (attribute list)
  val RawTable = data('RawTable) of
    'tempty | 'tcons ('Row, 'RawTable)

  // full table with "header" (attribute list)
  val Table = data('Table) of
    'table ('AttrL, 'RawTable)


  val getRaw = function('getRaw.>>('Table) -> 'RawTable)
  'getRaw ('table ('al, 'rt)) := 'rt

  val getAttrL = function('getAttrL.>>('Table) -> 'AttrL)
  'getAttrL ('table ('al, 'rt)) := 'al


  // function that assigns a field type to every field value  (underspecified)
  val fieldType = partial(function('fieldType.>>('Val) -> 'FType))

  // function that compares whether first field value is smaller than second field value
  // (underspecified)
  val lessThan = partial(function('lessThan.>>('Val, 'Val) -> 'Bool))

  // function that compares whether first field value is greater than second field value
  // (underspecified)
  val greaterThan = partial(function('greaterThan.>>('Val, 'Val) -> 'Bool))

  // check whether a table corresponds to a given type (functional notation)
  // does not yet check for whether the table type contains only unique attribute names!!
  // (but semantics should be possible to define in a sensible way without that requirement...)

  val matchingAttrL = function('matchingAttrL.>>('TType, 'AttrL) -> 'Bool)
    ('matchingAttrL ('ttempty, 'aempty) := true) |
    ('matchingAttrL ('ttcons ('a1, 'f, 'tt), 'acons ('a2, 'al)) := ('a1 === 'a2) && 'matchingAttrL ('tt, 'al)) |
    ('matchingAttrL ('tt, 'al) := false)

  val welltypedtable = function ('welltypedtable.>> ('TType, 'Table) -> 'Bool)
  'welltypedtable ('tt, 'table('al, 't)) := 'matchingAttrL('tt, 'al) && 'welltypedRawtable('tt, 't)

  val welltypedRawtable = function ('welltypedRawtable.>> ('TType, 'RawTable) -> 'Bool)
    ('welltypedRawtable ('tt, 'tempty) := true) |
    ('welltypedRawtable('tt, 'tcons('r, 't)) := 'welltypedRow('tt, 'r) && 'welltypedRawtable('tt, 't))

  val welltypedRow = function ('welltypedRow.>> ('TType, 'Row) -> 'Bool)
  ('welltypedRow ('ttempty, 'rempty) := true) |
    ('welltypedRow('ttcons('a, 'ft, 'tt), 'rcons('v, 'r)) := ('fieldType('v) === 'ft) && 'welltypedRow('tt, 'r)) |
    ('welltypedRow('tt, 'r) := false)

}
