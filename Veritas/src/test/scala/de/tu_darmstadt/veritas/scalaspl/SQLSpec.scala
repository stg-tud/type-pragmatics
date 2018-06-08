package de.tu_darmstadt.veritas.scalaspl

import de.tu_darmstadt.veritas.scalaspl.lang.{FailableAnnotations, ScalaSPLSpecification}

object SQLSpec extends ScalaSPLSpecification with FailableAnnotations {

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
    case table(_, rt) => rt
  }

  def getAttrL(t: Table): AttrL = t match {
    case table(al, _) => al
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
    case (_, _) => false
  }

  def welltypedRow(tType: TType, row: Row): Boolean = (tType, row) match {
    case (ttempty(), rempty()) => true
    case (ttcons(_, ft, tt), rcons(v, r)) => fieldType(v) == ft && welltypedRow(tt, r)
    case (_, _) => false
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
    case (_, aempty()) => false
    case (n1, acons(m, al1)) => (n1 == m) || attrIn(n1, al1)
  }

  def rowIn(r: Row, rt: RawTable): Boolean = (r, rt) match {
    case (_, tempty()) => false
    case (r1, tcons(r2, rt2)) => (r1 == r2) || rowIn(r1, rt2)
  }

  //projects a raw table to its first column
  //returns a raw table with exactly one column or tempty
  def projectFirstRaw(rt: RawTable): RawTable = rt match {
    case tempty() => tempty()
    case tcons(rempty(), rt1) => tcons(rempty(), projectFirstRaw(rt1))
    case tcons(rcons(f, _), rt1) => tcons(rcons(f, rempty()), projectFirstRaw(rt1))
  }

  //drops the first column of a raw table
  //returns a raw table with one column less than before or tempty
  def dropFirstColRaw(rt: RawTable): RawTable = rt match {
    case tempty() => tempty()
    case tcons(rempty(), rt1) => tcons(rempty(), dropFirstColRaw(rt1))
    case tcons(rcons(f, r), rt1) => tcons(r, dropFirstColRaw(rt1))
  }

  sealed trait OptRawTable

  case class noRawTable() extends OptRawTable

  case class someRawTable(rt: RawTable) extends OptRawTable

  def isSomeRawTable(ort: OptRawTable): Boolean = ort match {
    case noRawTable() => false
    case someRawTable(_) => true
  }

  @Partial
  def getRawTable(ort: OptRawTable): RawTable = ort match {
    case someRawTable(rt) => rt
  }

  //attaches a raw table with one column to the front of another raw table
  //returns a raw table with one column more, possibly not a welltyped one
  //(if the row counts of the input arguments differ)
  //assumes that both tables have the same row count!
  //include empty brackets after tempty such that the parser does not report an error
  //is treated exactly like tempty for fof-generation
  def attachColToFrontRaw(rt1: RawTable, rt2: RawTable): RawTable = (rt1, rt2) match {
    case (tempty(), tempty()) => tempty()
    case (tcons(rcons(f, rempty()), rt1r), tcons(r, rt2r)) => tcons(rcons(f, r), attachColToFrontRaw(rt1r, rt2r))
    case (_, _) => tcons(rempty(), tempty())
  }


  //definition: union removes duplicate rows
  //(but only between the two tables, not within a table!)
  //preserves row order of the two original raw tables
  def rawUnion(rt1: RawTable, rt2: RawTable): RawTable = (rt1, rt2) match {
    case (tempty(), rt2r) => rt2r
    case (rt1r, tempty()) => rt1r
    case (tcons(r1, rt1r), rt2r) =>
      val urt1rt2 = rawUnion(rt1r, rt2r)
      if (!rowIn(r1, rt2r))
        tcons(r1, urt1rt2)
      else
        urt1rt2
  }

  def rawIntersection(rt1: RawTable, rt2: RawTable): RawTable = (rt1, rt2) match {
    case (tempty(), rt2r) => tempty()
    case (rtr1, tempty()) => tempty()
    case (tcons(r1, tempty()), rtr2) =>
      if (rowIn(r1, rtr2))
        tcons(r1, tempty())
      else
        tempty()
    case (tcons(r1, rtr1), rtr2) =>
      val irt1rt2 = rawIntersection(rtr1, rtr2)
      if (rowIn(r1, rtr2))
        tcons(r1, irt1rt2)
      else irt1rt2
  }

  def rawDifference(rt1: RawTable, rt2: RawTable): RawTable = (rt1, rt2) match {
    case (rempty, rt2r) => tempty()
    case (rt1r, tempty()) => rt1r
    case (tcons(r1, tempty()), rtr2) =>
      if (!rowIn(r1, rtr2))
        tcons(r1, tempty())
      else tempty()
    case (tcons(r1, rtr1), rtr2) =>
      val drt1rt2 = rawDifference(rtr1, rtr2)
      if (!rowIn(r1, rtr2))
        tcons(r1, drt1rt2)
      else drt1rt2
  }

  sealed trait OptTable

  case class noTable() extends OptTable

  case class someTable(t: Table) extends OptTable

  def isSomeTable(ot: OptTable): Boolean = ot match {
    case noTable() => false
    case someTable(_) => true
  }

  @Partial
  def getTable(ot: OptTable): Table = ot match {
    case someTable(t) => t
  }


  sealed trait TStore

  case class emptyStore() extends TStore

  case class bindStore(n: Name, t: Table, rst: TStore)

  def lookupStore(n: Name, tst: TStore): OptTable = (n, tst) match {
    case (_, emptyStore()) => noTable()
    case (n1, bindStore(m, t, tsr)) =>
      if (n1 == m)
        someTable(t)
      else lookupStore(n1, tsr)
  }

  sealed trait TTContext

  case class emptyContext() extends TTContext

  case class bindContext(n: Name, tt: TType, ttr: TTContext) extends TTContext

  sealed trait OptTType

  case class noTType() extends OptTType

  case class someTType(tt: TType) extends OptTType

  def isSomeTType(ott: OptTType): Boolean = ott match {
    case noTType() => false
    case someTType(_) => true
  }

  @Partial
  def getTType(ott: OptTType): TType = ott match {
    case someTType(tt) => tt
  }

  def lookupContext(n: Name, ttc: TTContext): OptTType = (n, ttc) match {
    case (tn, emptyContext()) => noTType()
    case (tn, bindContext(tm, tt, ttcr)) =>
      if (tn == tm)
        someTType(tt)
      else lookupContext(tn, ttcr)
  }





}
