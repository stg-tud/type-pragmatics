package de.tu_darmstadt.veritas.scalaspl

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification

object SQLSpec extends ScalaSPLSpecification {

  // name of attributes and tables
  trait Name extends Expression

  // list of attribute names
  sealed trait AttrL extends Expression

  case class aempty() extends AttrL

  case class acons(hd: Name, tl: AttrL) extends AttrL

  @Recursive(0)
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
  @Static
  @Recursive(0, 1)
  def matchingAttrL(tt: TType, attrl: AttrL): Boolean = (tt, attrl) match {
    case (ttempty(), aempty()) => true
    case (ttcons(a1, _, ttr), acons(a2, al)) => (a1 == a2) && matchingAttrL(ttr, al)
    case (_, _) => false
  }

  @Static
  @Recursive(0, 1)
  def welltypedRow(tType: TType, row: Row): Boolean = (tType, row) match {
    case (ttempty(), rempty()) => true
    case (ttcons(_, ft, ttr), rcons(v, r)) => fieldType(v) == ft && welltypedRow(ttr, r)
    case (_, _) => false
  }

  @Static
  @Recursive(1)
  @Preservable
  def welltypedRawtable(tty: TType, rt: RawTable): Boolean = (tty, rt) match {
    case (_, tempty()) => true
    case (tt, tcons(r, t1)) => welltypedRow(tt, r) && welltypedRawtable(tt, t1)
  }

  @Static
  @Preservable
  def welltypedtable(tty: TType, t: Table): Boolean = (tty, t) match {
    case (tt, table(al, t1)) => matchingAttrL(tt, al) && welltypedRawtable(tt, t1)
  }

  //some auxiliary functions on raw tables (all not knowing anything about table types!)
  //the functions are intended to be used with well-typed tables!!

  @Dynamic
  @Recursive(1)
  def rowIn(r: Row, rt: RawTable): Boolean = (r, rt) match {
    case (_, tempty()) => false
    case (r1, tcons(r2, rt2)) => (r1 == r2) || rowIn(r1, rt2)
  }

  //projects a raw table to its first column
  //returns a raw table with exactly one column or tempty
  @Dynamic
  @PreservationProperty("projectFirstRawPreservesWelltypedRaw")
  @PreservationProperty("projectFirstRawPreservesRowCount")
  @Recursive(0)
  def projectFirstRaw(rt: RawTable): RawTable = rt match {
    case tempty() => tempty()
    case tcons(rempty(), rt1) => tcons(rempty(), projectFirstRaw(rt1))
    case tcons(rcons(f, _), rt1) => tcons(rcons(f, rempty()), projectFirstRaw(rt1))
  }

  //drops the first column of a raw table
  //returns a raw table with one column less than before or tempty
  @Dynamic
  @PreservationProperty("dropFirstColRawPreservesWelltypedRaw")
  @PreservationProperty("dropFirstColRawPreservesRowCount")
  @Recursive(0)
  def dropFirstColRaw(rt: RawTable): RawTable = rt match {
    case tempty() => tempty()
    case tcons(rempty(), rt1) => tcons(rempty(), dropFirstColRaw(rt1))
    case tcons(rcons(_, rr), rt1) => tcons(rr, dropFirstColRaw(rt1))
  }

  @FailableType
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

  @Dynamic
  @Recursive(0, 1)
  @Preservable
  def sameLength(rt1: RawTable, rt2: RawTable): Boolean = (rt1, rt2) match {
    case (tempty(), tempty()) => true
    case (tcons(_, tll), tcons(_, tlr)) => sameLength(tll, tlr)
    case (_, _) => false
  }

  //attaches a raw table with one column to the front of another raw table
  //returns a raw table with one column more, possibly not a welltyped one
  //(if the row counts of the input arguments differ)
  //assumes that both tables have the same row count!
  //include empty brackets after tempty such that the parser does not report an error
  //is treated exactly like tempty for fof-generation
  @Dynamic
  @PreservationProperty("attachColToFrontRawPreservesWellTypedRaw")
  @PreservationProperty("attachColToFrontRawPreservesRowCount")
  @Recursive(0, 1)
  def attachColToFrontRaw(rt1: RawTable, rt2: RawTable): RawTable = (rt1, rt2) match {
    case (tempty(), tempty()) => tempty()
    case (tcons(rcons(f, rempty()), rt1r), tcons(r, rt2r)) => tcons(rcons(f, r), attachColToFrontRaw(rt1r, rt2r))
    case (_, _) => tcons(rempty(), tempty())
  }


  //definition: union removes duplicate rows
  //(but only between the two tables, not within a table!)
  //preserves row order of the two original raw tables
  @Dynamic
  @PreservationProperty("rawUnionPreservesWellTypedRaw")
  @Recursive(0)
  def rawUnion(rtab1: RawTable, rtab2: RawTable): RawTable = (rtab1, rtab2) match {
    case (tempty(), rt) => rt
    case (tcons(r, rtr), rt1) =>
      val urt1rt2 = rawUnion(rtr, rt1)
      if (!rowIn(r, rt1))
        tcons(r, urt1rt2)
      else
        urt1rt2
  }

  @Dynamic
  @PreservationProperty("rawIntersectionPreservesWellTypedRaw")
  @Recursive(0)
  def rawIntersection(rtab1: RawTable, rtab2: RawTable): RawTable = (rtab1, rtab2) match {
    case (tempty(), _) => tempty()
    case (tcons(r, tempty()), rt1) =>
      if (rowIn(r, rt1))
        tcons(r, tempty())
      else
        tempty()
    case (tcons(r, rtr), rt1) =>
      val irt1rt2 = rawIntersection(rtr, rt1)
      if (rowIn(r, rt1))
        tcons(r, irt1rt2)
      else irt1rt2
  }

  @Dynamic
  @PreservationProperty("rawDifferencePreservesWellTypedRaw")
  @Recursive(0)
  def rawDifference(rtab1: RawTable, rtab2: RawTable): RawTable = (rtab1, rtab2) match {
    case (tempty(), _) => tempty()
    case (tcons(r, tempty()), rt2) =>
      if (!rowIn(r, rt2))
        tcons(r, tempty())
      else tempty()
    case (tcons(r, rtr), rt2) =>
      val drt1rt2 = rawDifference(rtr, rt2)
      if (!rowIn(r, rt2))
        tcons(r, drt1rt2)
      else drt1rt2
  }

  @FailableType
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

  case class bindStore(n: Name, t: Table, rst: TStore) extends TStore

  @Dynamic
  @ProgressProperty("successfulLookup")
  @PreservationProperty("welltypedLookup") // FIXME: In the strict sense, ``welltypedLookup`` is no preservation lemma
  @Recursive(1)
  def lookupStore(an: Name, tst: TStore): OptTable = (an, tst) match {
    case (_, emptyStore()) => noTable()
    case (n, bindStore(m, t, tsr)) =>
      if (n == m)
        someTable(t)
      else lookupStore(n, tsr)
  }

  sealed trait TTContext extends Context

  case class emptyContext() extends TTContext

  case class bindContext(n: Name, tt: TType, ttr: TTContext) extends TTContext


  @FailableType
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

  @Static
  @Recursive(1)
  def lookupContext(an: Name, ttc: TTContext): OptTType = (an, ttc) match {
    case (_, emptyContext()) => noTType()
    case (n, bindContext(m, tt, ttcr)) =>
      if (n == m)
        someTType(tt)
      else lookupContext(n, ttcr)
  }

  sealed trait Exp extends Expression

  case class constant(v: Val) extends Exp

  case class lookup(n: Name) extends Exp

  //predicates for where clauses of queries
  sealed trait Pred extends Expression

  case class ptrue() extends Pred

  case class and(p1: Pred, p2: Pred) extends Pred

  case class not(p: Pred) extends Pred

  case class eq(e1: Exp, e2: Exp) extends Pred

  case class gt(e1: Exp, e2: Exp) extends Pred

  case class lt(e1: Exp, e2: Exp) extends Pred

  // Query syntax
  sealed trait Select extends Expression

  case class all() extends Select

  case class list(attrL: AttrL) extends Select


  sealed trait Query extends Expression

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
  @FailableType
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

  @Dynamic
  @ProgressProperty("findColTypeImpliesfindCol")
  @PreservationProperty("findColPreservesWelltypedRaw")
  @PreservationProperty("findColPreservesRowCount")
  @Recursive(1)
  def findCol(a: Name, attrL: AttrL, rt: RawTable): OptRawTable = (a, attrL, rt) match {
    case (n, aempty(), _) => noRawTable()
    case (n, acons(n1, alr), rtr) =>
      if (n == n1)
        someRawTable(projectFirstRaw(rtr))
      else
        findCol(n, alr, dropFirstColRaw(rtr))
  }

  // for projection base case: projecting on an empty attribute list must yield a
  // table with as many empty rows as the rowcount of the given table
  @Dynamic
  @PreservationProperty("welltypedEmptyProjection")
  @PreservationProperty("projectEmptyColPreservesRowCount")
  @Recursive(0)
  def projectEmptyCol(rt: RawTable): RawTable = rt match {
    case tempty() => tempty()
    case tcons(_, t) => tcons(rempty(), projectEmptyCol(t))
  }

  // arguments: select-list table-list table-rows
  @Dynamic
  @ProgressProperty("projectColsProgress")
  @PreservationProperty("projectColsWelltypedWithSelectType")
  @PreservationProperty("projectColsPreservesRowCount")
  @Recursive(0)
  def projectCols(attrl1: AttrL, attrl2: AttrL, rtable: RawTable): OptRawTable = (attrl1, attrl2, rtable) match {
    case (aempty(), _, rt) => someRawTable(projectEmptyCol(rt))
    case (acons(n, al2), al1, rt) =>
      val col = findCol(n, al1, rt)
      val rest = projectCols(al2, al1, rt)
      if (isSomeRawTable(col) && isSomeRawTable(rest))
        someRawTable(attachColToFrontRaw(getRawTable(col), getRawTable(rest)))
      else
        noRawTable()
  }

  @Dynamic
  @ProgressProperty("projectTableProgress")
  @PreservationProperty("projectTableWelltypedWithSelectType")
  //@PreservationProperty("projectTypeAttrLMatchesAttrL") // FIXME: projectTypeAttrLMatchesAttrL is no preservation lemma
  def projectTable(s: Select, tab: Table): OptTable = (s, tab) match {
    case (all(), t) => someTable(t)
    case (list(al), t) =>
      val projected = projectCols(al, getAttrL(t), getRaw(t))
      if (isSomeRawTable(projected))
        someTable(table(al, getRawTable(projected)))
      else
        noTable()
  }

  @FailableType
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

  @Dynamic
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
  @Dynamic
  @Recursive(0)
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
  @Dynamic
  @PreservationProperty("filterRowsPreservesTable")
  @Recursive(0)
  def filterRows(rt: RawTable, attrL: AttrL, pred: Pred): RawTable = (rt, attrL, pred) match {
    case (tempty(), _, _) => tempty()
    case (tcons(r, rtr), al, p) =>
      val rts = filterRows(rtr, al, p)
      if (filterSingleRow(p, al, r))
        tcons(r, rts)
      else
        rts
  }

  @Dynamic
  @PreservationProperty("filterPreservesType")
  def filterTable(t: Table, pred: Pred): Table = (t, pred) match {
    case (table(al, rt), p) => table(al, filterRows(rt, al, p))
  }

  @Dynamic
  @ProgressProperty("Progress")
  @PreservationProperty("Preservation")
  @Recursive(0)
  def reduce(query: Query, tst: TStore): OptQuery = (query, tst) match {
    case (tvalue(_), _) => noQuery()
    case (selectFromWhere(s, n, p), ts) =>
      val maybeTable = lookupStore(n, ts)
      if (isSomeTable(maybeTable)) {
        val filtered = filterTable(getTable(maybeTable), p)
        val maybeSelected = projectTable(s, filtered)
        if (isSomeTable(maybeSelected))
          someQuery(tvalue(getTable(maybeSelected)))
        else noQuery()
      }
      else
        noQuery()
    case (Union(tvalue(t1), tvalue(t2)), ts) =>
      someQuery(tvalue(table(getAttrL(t1), rawUnion(getRaw(t1), getRaw(t2)))))
    case (Union(tvalue(t), q2), ts) =>
      val q2reduce = reduce(q2, ts)
      if (isSomeQuery(q2reduce))
        someQuery(Union(tvalue(t), getQuery(q2reduce)))
      else
        noQuery()
    case (Union(q1, q2), ts) =>
      val q1reduce = reduce(q1, ts)
      if (isSomeQuery(q1reduce))
        someQuery(Union(getQuery(q1reduce), q2))
      else
        noQuery()
    case (Intersection(tvalue(t1), tvalue(t2)), ts) =>
      someQuery(tvalue(table(getAttrL(t1), rawIntersection(getRaw(t1), getRaw(t2)))))
    case (Intersection(tvalue(t), q2), ts) =>
      val q2reduce = reduce(q2, ts)
      if (isSomeQuery(q2reduce))
        someQuery(Intersection(tvalue(t), getQuery(q2reduce)))
      else
        noQuery()
    case (Intersection(q1, q2), ts) =>
      val q1reduce = reduce(q1, ts)
      if (isSomeQuery(q1reduce))
        someQuery(Intersection(getQuery(q1reduce), q2))
      else
        noQuery()
    case (Difference(tvalue(t1), tvalue(t2)), ts) =>
      someQuery(tvalue(table(getAttrL(t1), rawDifference(getRaw(t1), getRaw(t2)))))
    case (Difference(tvalue(t), q2), ts) =>
      val q2reduce = reduce(q2, ts)
      if (isSomeQuery(q2reduce))
        someQuery(Difference(tvalue(t), getQuery(q2reduce)))
      else
        noQuery()
    case (Difference(q1, q2), ts) =>
      val q1reduce = reduce(q1, ts)
      if (isSomeQuery(q1reduce))
        someQuery(Difference(getQuery(q1reduce), q2))
      else
        noQuery()
  }

  @FailableType
  sealed trait OptFType

  case class noFType() extends OptFType

  case class someFType(ft: FType) extends OptFType

  def isSomeFType(oft: OptFType): Boolean = oft match {
    case noFType() => false
    case someFType(a) => true
  }

  @Partial
  def getFType(oft: OptFType): FType = oft match {
    case someFType(a) => a
  }

  @Static
  @Recursive(1)
  def findColType(an: Name, tt: TType): OptFType = (an, tt) match {
    case (n, ttempty()) => noFType()
    case (n, ttcons(a, ft, ttr)) =>
      if (n == a)
        someFType(ft)
      else
        findColType(n, ttr)
  }

  @Static
  @Recursive(0)
  def projectTypeAttrL(attrl: AttrL, tty: TType): OptTType = (attrl, tty) match {
    case (aempty(), tt) => someTType(ttempty())
    case (acons(a, alr), tt) =>
      val ft = findColType(a, tt)
      val tprest = projectTypeAttrL(alr, tt)
      if (isSomeFType(ft) && isSomeTType(tprest))
        someTType(ttcons(a, getFType(ft), getTType(tprest)))
      else
        noTType()
  }

  @Static
  def projectType(sel: Select, tt: TType): OptTType = (sel, tt) match {
    case (all(), tt1) => someTType(tt1)
    case (list(al), tt1) => projectTypeAttrL(al, tt1)
  }

  @Static
  @Recursive(0)
  def typeOfExp(e: Exp, tty: TType): OptFType = (e, tty) match {
    case (constant(fv), tt) => someFType(fieldType(fv))
    case (lookup(n), ttempty()) => noFType()
    case (lookup(n), ttcons(a2, ft, ttr)) =>
      if (n == a2)
        someFType(ft)
      else
        typeOfExp(lookup(n), ttr)
  }


  @Static
  @Recursive(0)
  def tcheckPred(pred: Pred, tType: TType): Boolean = (pred, tType) match {
    case (ptrue(), tt) => true
    case (and(p1, p2), tt) => tcheckPred(p1, tt) && tcheckPred(p2, tt)
    case (not(p), tt) => tcheckPred(p, tt)
    case (eq(e1, e2), tt) =>
      val t1 = typeOfExp(e1, tt)
      val t2 = typeOfExp(e2, tt)
      isSomeFType(t1) && isSomeFType(t2) && (getFType(t1) == getFType(t2))
    case (gt(e1, e2), tt) =>
      val t1 = typeOfExp(e1, tt)
      val t2 = typeOfExp(e2, tt)
      isSomeFType(t1) && isSomeFType(t2) && (getFType(t1) == getFType(t2))
    case (lt(e1, e2), tt) =>
      val t1 = typeOfExp(e1, tt)
      val t2 = typeOfExp(e2, tt)
      isSomeFType(t1) && isSomeFType(t2) && (getFType(t1) == getFType(t2))
  }

  //axioms on behavior of table type context
  @Axiom
  def TTTContextDuplicate(x: Name, y: Name, Tx: TType, Ty: TType, C: TTContext, e: Query, T: TType): Unit = {
    require(x == y)
    require(bindContext(x, Tx, bindContext(y, Ty, C)) |- e :: T)
  } ensuring (bindContext(x, Tx, C) |- e :: T)

  @Axiom
  def TTTContextSwap(x: Name, y: Name, Tx: TType, Ty: TType, C: TTContext, e: Query, T: TType): Unit = {
    require(x != y)
    require(bindContext(x, Tx, bindContext(y, Ty, C)) |- e :: T)
  } ensuring(bindContext(y, Ty, bindContext(x, Tx, C)) |- e :: T)

  @Axiom
  def Ttvalue(t: Table, TTC: TTContext, TT: TType): Unit = {
    require(welltypedtable(TT, t))
  } ensuring(TTC |- tvalue(t) :: TT)

  @Axiom
  def TSelectFromWhere(tn: Name, TTC: TTContext, TT: TType, p: Pred, sel: Select, TTr: TType): Unit = {
    require(lookupContext(tn, TTC) == someTType(TT))
    require(tcheckPred(p, TT))
    require(projectType(sel, TT) == someTType(TTr))
  } ensuring(TTC |- selectFromWhere(sel, tn, p) :: TTr)

  @Axiom
  def TUnion(q1: Query, q2: Query, TT: TType, TTC: TTContext): Unit = {
    require(TTC |- q1 :: TT)
    require(TTC |- q2 :: TT)
  } ensuring(TTC |- Union(q1, q2) :: TT)

  @Axiom
  def TIntersection(q1: Query, q2: Query, TT: TType, TTC: TTContext): Unit = {
    require(TTC |- q1 :: TT)
    require(TTC |- q2 :: TT)
  } ensuring(TTC |- Intersection(q1, q2) :: TT)

  @Axiom
  def TDifference(q1: Query, q2: Query, TT: TType, TTC: TTContext): Unit = {
    require(TTC |- q1 :: TT)
    require(TTC |- q2 :: TT)
  } ensuring(TTC |- Difference(q1, q2) :: TT)

  // type inversion axioms
  @Axiom
  def Ttvalue_inv(t: table, TTC: TTContext, TT: TType): Unit = {
    require(TTC |- tvalue(t) :: TT)
  } ensuring(welltypedtable(TT, t))

  @Axiom
  def TSelectFromWhere_inv(tn: Name, TTC: TTContext, p: Pred, sel: Select, TTr: TType): Unit = {
    require(TTC |- selectFromWhere(sel, tn, p) :: TTr)
  } ensuring(exists((TT: TType) => lookupContext(tn, TTC) == someTType(TT) && tcheckPred(p, TT) && projectType(sel, TT) == someTType(TTr)))

  @Axiom
  def TUnion_inv1(q1: Query, q2: Query, TT: TType, TTC: TTContext): Unit = {
    require(TTC |- Union(q1, q2) :: TT)
  } ensuring(TTC |- q1 :: TT)

  @Axiom
  def TUnion_inv2(q1: Query, q2: Query, TT: TType, TTC: TTContext): Unit = {
    require(TTC |- Union(q1, q2) :: TT)
  } ensuring(TTC |- q2 :: TT)

  @Axiom
  def TIntersection_inv1(q1: Query, q2: Query, TT: TType, TTC: TTContext): Unit = {
    require(TTC |- Intersection(q1, q2) :: TT)
  } ensuring(TTC |- q1 :: TT)

  @Axiom
  def TIntersection_inv2(q1: Query, q2: Query, TT: TType, TTC: TTContext): Unit = {
    require(TTC |- Intersection(q1, q2) :: TT)
  } ensuring(TTC |- q2 :: TT)

  @Axiom
  def TDifference_inv1(q1: Query, q2: Query, TT: TType, TTC: TTContext): Unit = {
    require(TTC |- Difference(q1, q2) :: TT)
  } ensuring(TTC |- q1 :: TT)

  @Axiom
  def TDifference_inv2(q1: Query, q2: Query, TT: TType, TTC: TTContext): Unit = {
    require(TTC |- Difference(q1, q2) :: TT)
  } ensuring(TTC |- q2 :: TT)


  // determines whether a given TTContext is consistent with a given TStore
  // and whether the table in the store is well-typed with regard to the table type in the context
  // design decision: require bindings to appear in exactly the SAME ORDER! (simpler?)
  @Static
  @Recursive(0, 1)
  def storeContextConsistent(ts: TStore, ttc: TTContext): Boolean = (ts, ttc) match {
    case (emptyStore(), emptyContext()) => true
    case (bindStore(tn1, t, tsr), bindContext(tn2, tt, ttcr)) =>
      tn1 == tn2 && welltypedtable(tt, t) && storeContextConsistent(tsr, ttcr)
    case (_, _) => false
  }

  // LEMMAS BEGIN
  //PROGRESS
  @Property
  def Progress(ts: TStore, ttc: TTContext, q: Query): Unit = {
    require(storeContextConsistent(ts, ttc))
    require(!isValue(q))
    require(exists((tt1: TType) => ttc |- q :: tt1))
  } ensuring exists((qr: Query) => reduce(q, ts) == someQuery(qr))

  // auxiliary lemmas for progress proof
  @Property
  def successfulLookup(ttc: TTContext, ts: TStore, ref: Name, tt: TType): Unit = {
    require(storeContextConsistent(ts, ttc))
    require(lookupContext(ref, ttc) == someTType(tt))
  } ensuring exists((t: Table) => lookupStore(ref, ts) == someTable(t))

  @Property
  def welltypedLookup(ttc: TTContext, ts: TStore, ref: Name, tt: TType, t: Table): Unit = {
    require(storeContextConsistent(ts, ttc))
    require(lookupContext(ref, ttc) == someTType(tt))
    require(lookupStore(ref, ts) == someTable(t))
  } ensuring welltypedtable(tt, t)

  @Property
  def filterPreservesType(tt: TType, t: Table, result: Table, p: Pred): Unit = {
    require(welltypedtable(tt, t))
    require(filterTable(t, p) == result)
  } ensuring welltypedtable(tt, result)

  @Property
  def projectTableProgress(tt: TType, t: Table, s: Select, tt2: TType): Unit = {
    require(welltypedtable(tt, t))
    require(projectType(s, tt) == someTType(tt2))
  } ensuring exists((t2: Table) => projectTable(s, t) == someTable(t2))

  @Property
  def filterRowsPreservesTable(tt: TType, rt: RawTable, rt2: RawTable, al: AttrL, p: Pred): Unit = {
    require(welltypedRawtable(tt, rt))
    require(filterRows(rt, al, p) == rt2)
  } ensuring welltypedRawtable(tt, rt2)

  @Property
  def projectColsProgress(tt: TType, alt: AttrL, rt: RawTable, al: AttrL, tt2: TType): Unit = {
    require(welltypedRawtable(tt, rt))
    require(matchingAttrL(tt, alt))
    //require(projectType(list(al2), tt) == someTType(tt2)) NOTE: expanded this to:
    require(projectTypeAttrL(al, tt) == someTType(tt2))
  } ensuring exists((rt2: RawTable) => projectCols(al, alt, rt) == someRawTable(rt2))

  @Property
  def findColTypeImpliesfindCol(tt: TType, al: AttrL, rt: RawTable, n: Name, ft: FType): Unit = {
    require(welltypedRawtable(tt, rt))
    require(matchingAttrL(tt, al))
    require(findColType(n, tt) == someFType(ft))
  } ensuring exists((rt2: RawTable) => findCol(n, al, rt) == someRawTable(rt2))

  @Property
  def dropFirstColRawPreservesWelltypedRaw(tt: TType, n: Name, ft: FType, ttrest: TType, rt: RawTable, rt1: RawTable): Unit = {
    require(tt == ttcons(n, ft, ttrest)) // |tt| > 0
    require(welltypedRawtable(tt, rt))
    require(dropFirstColRaw(rt) == rt1)
  } ensuring welltypedRawtable(ttrest, rt1)

  //PRESERVATION

  // union, intersection, difference preserve well-typedness of raw tables
  @Property
  def rawUnionPreservesWellTypedRaw(rt: RawTable, rt1: RawTable, result: RawTable, tt: TType): Unit = {
    require(welltypedRawtable(tt, rt))
    require(welltypedRawtable(tt, rt1))
    require(rawUnion(rt, rt1) == result)
  } ensuring welltypedRawtable(tt, result)

  @Property
  def rawIntersectionPreservesWellTypedRaw(rt: RawTable, rt1: RawTable, result: RawTable, tt: TType): Unit = {
    require(welltypedRawtable(tt, rt))
    require(welltypedRawtable(tt, rt1))
    require(rawIntersection(rt, rt1) == result)
  } ensuring welltypedRawtable(tt, result)

  @Property
  def rawDifferencePreservesWellTypedRaw(rt: RawTable, rt1: RawTable, result: RawTable, tt: TType): Unit = {
    require(welltypedRawtable(tt, rt))
    require(welltypedRawtable(tt, rt1))
    require(rawDifference(rt, rt1) == result)
  } ensuring welltypedRawtable(tt, result)

  @Property
  def projectTypeAttrLMatchesAttrL(al: AttrL, tt: TType, tt2: TType): Unit = {
    require(projectTypeAttrL(al, tt) == someTType(tt2))
  } ensuring matchingAttrL(tt2, al)

  @Property
  def welltypedEmptyProjection(rt: RawTable, rt1: RawTable, tt: TType): Unit = {
    require(tt == ttempty())
    require(projectEmptyCol(rt) == rt1)
  } ensuring welltypedRawtable(tt, rt1)

  @Property
  def projectFirstRawPreservesWelltypedRaw(rt: RawTable, rt1: RawTable,
                                           tt: TType, tt1: TType,
                                           a: Name, ct: FType, ttrest: TType): Unit = {
    require(tt == ttcons(a, ct, ttrest))
    require(tt1 == ttcons(a, ct, ttempty()))
    require(welltypedRawtable(tt, rt))
    require(projectFirstRaw(rt) == rt1)
  } ensuring welltypedRawtable(tt1, rt1)

  @Property
  def findColPreservesWelltypedRaw(n: Name, al: AttrL, rt: RawTable,
                                   tt: TType, tt2: TType, ft: FType, rt2: RawTable): Unit = {
    require(welltypedRawtable(tt, rt))
    require(matchingAttrL(tt, al))
    require(findColType(n, tt) == someFType(ft))
    require(findCol(n, al, rt) == someRawTable(rt2))
    require(tt2 == ttcons(n, ft, ttempty()))
  } ensuring welltypedRawtable(tt2, rt2)

  @Property
  def attachColToFrontRawPreservesWellTypedRaw(tt1: TType, name1: Name, ft1: FType,
                                               tt2: TType, tt3: TType,
                                               rt: RawTable, rt1: RawTable, rt2: RawTable): Unit = {
    // |tt1| == 1
    require(tt1 == ttcons(name1, ft1, ttempty()))
    require(sameLength(rt, rt1))
    require(welltypedRawtable(tt1, rt))
    require(welltypedRawtable(tt2, rt1))
    require(attachColToFrontRaw(rt, rt1) == rt2)
    require(tt3 == ttcons(name1, ft1, tt2))
  } ensuring welltypedRawtable(tt3, rt2)

  @Property
  def attachColToFrontRawPreservesRowCount(tt1: TType, a: Name, ct: FType,
                                           rt: RawTable, rt1: RawTable, rt2: RawTable): Unit = {
    require(tt1 == ttcons(a, ct, ttempty()))
    require(welltypedRawtable(tt1, rt))
    require(sameLength(rt, rt1))
    require(attachColToFrontRaw(rt, rt1) == rt2)
  } ensuring sameLength(rt, rt2)

  @Property
  def projectFirstRawPreservesRowCount(rt: RawTable, rt1: RawTable): Unit = {
    require(projectFirstRaw(rt) == rt1)
  } ensuring sameLength(rt, rt1)

  @Property
  def dropFirstColRawPreservesRowCount(rt: RawTable, rt1: RawTable): Unit = {
    require(dropFirstColRaw(rt) == rt1)
  } ensuring sameLength(rt, rt1)

  @Property
  def findColPreservesRowCount(n: Name, al: AttrL, rt: RawTable, rt1: RawTable): Unit = {
    require(findCol(n, al, rt) == someRawTable(rt1))
  } ensuring sameLength(rt, rt1)

  @Property
  def projectEmptyColPreservesRowCount(rt: RawTable, rt1: RawTable): Unit = {
    require(projectEmptyCol(rt) == rt1)
  } ensuring sameLength(rt, rt1)

  @Property
  def projectColsPreservesRowCount(tt: TType, tt1: TType, al: AttrL, al1: AttrL, rt: RawTable, rt1: RawTable): Unit = {
    require(projectTypeAttrL(al, tt) == someTType(tt1))
    require(projectCols(al, al1, rt) == someRawTable(rt1))
    require(welltypedRawtable(tt, rt))
    require(matchingAttrL(tt, al1))
  } ensuring sameLength(rt, rt1)

  @Property
  def projectColsWelltypedWithSelectType(al: AttrL, tal: AttrL, rt: RawTable, rt1: RawTable, tt: TType, tt1: TType): Unit = {
    require(welltypedRawtable(tt, rt))
    require(matchingAttrL(tt, tal))
    require(projectTypeAttrL(al, tt) == someTType(tt1))
    require(projectCols(al, tal, rt) == someRawTable(rt1))
  } ensuring welltypedRawtable(tt1, rt1)

  @Property
  def projectTableWelltypedWithSelectType(s: Select, t: Table, t1: Table, tt: TType, tt1: TType): Unit = {
    require(welltypedtable(tt, t))
    require(projectType(s, tt) == someTType(tt1))
    require(projectTable(s, t) == someTable(t1))
  } ensuring welltypedtable(tt1, t1)

  @Property
  def Preservation(ttc: TTContext, ts: TStore, q: Query, qr: Query, tt: TType): Unit = {
    require(storeContextConsistent(ts, ttc))
    require(ttc |- q :: tt)
    require(reduce(q, ts) == someQuery(qr))
  } ensuring(ttc |- qr :: tt)
  // LEMMAS END
}