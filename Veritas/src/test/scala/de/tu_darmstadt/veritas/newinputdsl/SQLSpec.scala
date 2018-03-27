package de.tu_darmstadt.veritas.newinputdsl

import de.tu_darmstadt.veritas.newinputdsl.lang.{FailableAnnotations, SPLSpecification}

object SQLSpec extends SPLSpecification with FailableAnnotations {

  // name of attributes and tables
  trait Name extends Expression

  // list of attribute names
  sealed trait AttrL extends Expression
  case class aempty() extends AttrL
  case class acons(hd: Name, tl: AttrL) extends AttrL

  def append(atl1: AttrL, atl2: AttrL): AttrL = (atl1, atl2) match {
    case (aempty(), atl) => atl
    case (acons(name, atlr), atl) => acons(name, append(atlr, atl))
  }

  trait FType extends Type

  // type of a table (table schema)
  sealed trait TType extends Type
  case class ttempty() extends TType
  case class ttcons(n: Name, ft: FType, tt: TType) extends TType

  // Value for a field (underspecified)
  trait Val extends Expression

  // table row, list of field values (with at least one cell/field per construction!)
  sealed trait Row extends Expression
  case class rempty() extends Row
  case class rcons(v: Val, r: Row) extends Row

  // table matrix (list of rows), without "header" (attribute list)
  sealed trait RawTable extends Expression
  case class tempty() extends RawTable
  case class tcons(r: Row, rt: RawTable) extends RawTable

  // full table with "header" (attribute list)
  sealed trait Table extends Expression
  case class table(a: AttrL, rt: RawTable) extends Table

  def getRaw(t: Table): RawTable = t match {
    case table(a, rt) => rt
  }

  def getAttrL(t: Table): AttrL = t match {
    case table(al, rt) => al
  }

  // function that assigns a field type to every field value  (underspecified)
  def fieldType(v: Val): FType = ???

  // function that compares whether first field value is smaller than second field value
  // (underspecified)
  def lessThan(v1: Val, v2: Val): Boolean = ???

  // function that compares whether first field value is greater than second field value
  // (underspecified)
  def greaterThan(v1: Val, v2: Val): Boolean = ???

  // check whether a table corresponds to a given type (functional notation)
  // does not yet check for whether the table type contains only unique attribute names!!
  // (but semantics should be possible to define in a sensible way without that requirement...)
  def matchingAttrL(tt: TType, attrl: AttrL): Boolean = (tt, attrl) match {
    case (ttempty(), aempty()) => true
    case (ttcons(a1, f, ttr), acons(a2, al)) => (a1 == a2) && matchingAttrL(ttr, al)
    case (tt1, al) => false
  }

  def welltypedRow(tType: TType, row: Row): Boolean = (tType, row) match {
    case (ttempty(), rempty()) => true
    case (ttcons(a, ft, tt), rcons(v, r)) => fieldType(v) == ft && welltypedRow(tt, r)
    case (tt, r) => false
  }

  def welltypedRawtable(tt: TType, rt: RawTable): Boolean = (tt, rt) match {
    case (tt1, tempty()) => true
    case (tt1, tcons(r, t1)) => welltypedRow(tt1, r) && welltypedRawtable(tt1, t1)
  }

  def welltypedtable(tt: TType, t: Table): Boolean = (tt, t) match {
    case (tt1, table(al, t1)) => matchingAttrL(tt1, al) && welltypedRawtable(tt1, t1)
  }

  //some auxiliary functions on raw tables (all not knowing anything about table types!)
  //the functions are intended to be used with well-typed tables!!
  def attrIn(n: Name, al: AttrL): Boolean = (n, al) match {
    case (n1, aempty()) => false
    case (n1, acons(m, al1)) => (n1 == m) || attrIn(n1, al1)
  }

  def rowIn(r: Row, rt: RawTable): Boolean = (r, rt) match {
    case (r1, tempty()) => false
    case (r1, tcons(r2, rt2)) => (r1 == r2) || rowIn(r1, rt2)
  }

  //projects a raw table to its first column
  //returns a raw table with exactly one column or tempty
  def projectFirstRaw(rt: RawTable): RawTable = rt match {
    case tempty() => tempty()
    case tcons(rempty(), rt1) => tcons(rempty(), projectFirstRaw(rt1))
    case tcons(rcons(f, r), rt1) => tcons(rcons(f, rempty()), projectFirstRaw(rt1))
  }

  //drops the first column of a raw table
  //returns a raw table with one column less than before or tempty
  def dropFirstColRaw(rt: RawTable): RawTable = rt match {
    case tempty() => tempty()
    case tcons(rempty(), rt1) => tcons(rempty(), dropFirstColRaw(rt1))
    case tcons(rcons(f, r), rt1) => tcons(r, dropFirstColRaw(rt1))
  }

  

}
