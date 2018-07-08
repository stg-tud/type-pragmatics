package de.tu_darmstadt.veritas.scalaspl

import de.tu_darmstadt.veritas.scalaspl.lang.{FailableAnnotations, ScalaSPLSpecification}
import javax.xml.ws.RequestWrapper

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
    case (ttcons(a1, _, ttr), acons(a2, al)) => (a1 == a2) && matchingAttrL(ttr, al)
    case (_, _) => false
  }

  def welltypedRow(tType: TType, row: Row): Boolean = (tType, row) match {
    case (ttempty(), rempty()) => true
    case (ttcons(_, ft, tt), rcons(v, r)) => fieldType(v) == ft && welltypedRow(tt, r)
    case (_, _) => false
  }

  def welltypedRawtable(tt: TType, rt: RawTable): Boolean = (tt, rt) match {
    case (_, tempty()) => true
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
    case tcons(rcons(_, r), rt1) => tcons(r, dropFirstColRaw(rt1))
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
    case (tempty(), _) => tempty()
    case (_, tempty()) => tempty()
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
    case (rempty(), _) => tempty()
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
    case (_, emptyContext()) => noTType()
    case (tn, bindContext(tm, tt, ttcr)) =>
      if (tn == tm)
        someTType(tt)
      else lookupContext(tn, ttcr)
  }

  sealed trait Exp

  case class constant(v: Val) extends Exp

  case class lookup(n: Name) extends Exp

  //predicates for where clauses of queries
  sealed trait Pred

  case class ptrue() extends Pred

  case class and(p1: Pred, p2: Pred) extends Pred

  case class not(p: Pred) extends Pred

  case class eq(e1: Exp, e2: Exp) extends Exp

  case class gt(e1: Exp, e2: Exp) extends Exp

  case class lt(e1: Exp, e2: Exp) extends Exp

  // Query syntax
  sealed trait Select

  case class all() extends Select

  case class list(attrL: AttrL) extends Select


  sealed trait Query

  case class tvalue(t: Table) extends Query

  case class selectFromWhere(s: Select, name: Name, pred: Pred) extends Query

  case class Union(q1: Query, q2: Query) extends Query

  case class Intersection(q1: Query, q2: Query) extends Query

  case class Difference(q1: Query, q2: Query) extends Query

  def isValue(q: Query): Boolean = q match {
    case tvalue(_) => true
    case selectFromWhere(_, _, _) => false
    case Union(_, _) => false
    case Intersection(_, _) => false
    case Difference(_, _) => false
  }


  //functions for semantics of SQL
  sealed trait OptQuery

  case class noQuery() extends OptQuery

  case class someQuery(q: Query) extends OptQuery

  def isSomeQuery(oq: OptQuery): Boolean = oq match {
    case noQuery() => false
    case someQuery(_) => true
  }

  @Partial
  def getQuery(oq: OptQuery): Query = oq match {
    case someQuery(q) => q
  }

  def findCol(n: Name, attrL: AttrL, rt: RawTable): OptRawTable = (n, attrL, rt) match {
    case (a, aempty(), _) => noRawTable()
    case (a, acons(a2, al), rtr) =>
      if (a == a2)
        someRawTable(projectFirstRaw(rtr))
      else
        findCol(a, al, dropFirstColRaw(rtr))
  }

  // for projection base case: projecting on an empty attribute list must yield a
  // table with as many empty rows as the rowcount of the given table
  def projectEmptyCol(rt: RawTable): RawTable = rt match {
    case tempty() => tempty()
    case tcons(_, t) => tcons(rempty(), projectEmptyCol(t))
  }

  // arguments: select-list table-list table-rows
  def projectCols(al1: AttrL, al2: AttrL, rt: RawTable): OptRawTable = (al1, al2, rt) match {
    case (aempty(), _, rtr) => someRawTable(projectEmptyCol(rtr))
    case (acons(a, alr), al, rtr) =>
      val col = findCol(a, al, rtr)
      val rest = projectCols(alr, al, rtr)
      if (isSomeRawTable(col) && isSomeRawTable(rest))
        someRawTable(attachColToFrontRaw(getRawTable(col), getRawTable(rest)))
      else
        noRawTable()
  }

  def projectTable(s: Select, t: Table): OptTable = (s, t) match {
    case (all(), table(al, rt)) => someTable(table(al, rt))
    case (list(alr), table(al, rt)) =>
      val projected = projectCols(alr, al, rt)
      if (isSomeRawTable(projected))
        someTable(table(alr, getRawTable(projected)))
      else
        noTable()
  }

  sealed trait OptVal

  case class noVal() extends OptVal

  case class someVal(v: Val) extends OptVal

  def isSomeVal(ov: OptVal): Boolean = ov match {
    case noVal() => false
    case someVal(_) => true
  }

  @Partial
  def getVal(ov: OptVal): Val = ov match {
    case someVal(v) => v
  }

  def evalExpRow(e: Exp, attrL: AttrL, row: Row): OptVal = (e, attrL, row) match {
    case (constant(v), _, _) => someVal(v)
    case (lookup(a), acons(a2, al), rcons(v, r)) =>
      if (a == a2)
        someVal(v)
      else
        evalExpRow(lookup(a), al, r)
    case (_, _, _) => noVal()
  }

  // returns true iff predicate succeeds on row
  // returns false if predicate evaluates to false or if predicate evaluation fails
  def filterSingleRow(p: Pred, attrL: AttrL, row: Row): Boolean = (p, attrL, row) match {
    case (ptrue(), _, _) => true
    case (and(p1, p2), al, r) => filterSingleRow(p1, al, r) && filterSingleRow(p2, al, r)
    case (not(pr), al, r) => !filterSingleRow(pr, al, r)
    case (eq(e1, e2), al, r) =>
      val v1 = evalExpRow(e1, al, r)
      val v2 = evalExpRow(e2, al, r)
      isSomeVal(v1) && isSomeVal(v2) && getVal(v1) == getVal(v2)
    case (gt(e1, e2), al, r) =>
      val v1 = evalExpRow(e1, al, r)
      val v2 = evalExpRow(e2, al, r)
      isSomeVal(v1) && isSomeVal(v2) && greaterThan(getVal(v1), getVal(v2))
    case (lt(e1, e2), al, r) =>
      val v1 = evalExpRow(e1, al, r)
      val v2 = evalExpRow(e2, al, r)
      isSomeVal(v1) && isSomeVal(v2) && lessThan(getVal(v1), getVal(v2))
  }

  // filter rows that satisfy pred
  def filterRows(rt: RawTable, attrL: AttrL, pred: Pred): RawTable = (rt, attrL, pred) match {
    case (tempty(), _, _) => tempty()
    case (tcons(r, rtr), al, p) =>
      val rts = filterRows(rtr, al, p)
      if (filterSingleRow(p, al, r))
        tcons(r, rts)
      else
        rts
  }

  def filterTable(t: Table, pred: Pred): Table = (t, pred) match {
    case (table(al, rt), p) => table(al, filterRows(rt, al, p))
  }

  def reduce(query: Query, tst: TStore): OptQuery = (query, tst) match {
    case (tvalue(_), _) => noQuery()
    case (selectFromWhere(sel, name, pred), ts) =>
      val maybeTable = lookupStore(name, ts)
      if (isSomeTable(maybeTable)) {
        val filtered = filterTable(getTable(maybeTable), pred)
        val maybeSelected = projectTable(sel, filtered)
        if (isSomeTable(maybeSelected))
          someQuery(tvalue(getTable(maybeSelected)))
        else noQuery()
      }
      else
        noQuery()
    //TODO

  }


}
