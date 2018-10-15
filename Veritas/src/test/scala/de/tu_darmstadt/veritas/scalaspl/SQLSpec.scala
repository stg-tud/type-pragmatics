package de.tu_darmstadt.veritas.scalaspl

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification

object SQLSpec extends ScalaSPLSpecification {

  // name of attributes and tables
  trait Name extends Expression

  // natural numbers for preservation proof
  sealed trait nat extends Expression
  case class zero() extends nat
  case class succ(n: nat) extends nat

  def nonzero(n: nat): Boolean = n match {
    case zero() => false
    case succ(_) => true
  }

  // list of attribute names
  sealed trait AttrL extends Expression

  case class aempty() extends AttrL

  case class acons(hd: Name, tl: AttrL) extends AttrL

  def append(atl1: AttrL, atl2: AttrL): AttrL = (atl1, atl2) match {
    case (aempty(), atl) => atl
    case (acons(name, atlr), atl) => acons(name, append(atlr, atl))
  }

  def attrListLength(al: AttrL): nat = al match {
    case aempty() => zero()
    case acons(_, altail) => succ(attrListLength(altail))
  }

  trait FType extends Type

  // type of a table (table schema)
  sealed trait TType extends Type

  case class ttempty() extends TType

  case class ttcons(n: Name, ft: FType, tt: TType) extends TType

  def getFirstName(tt: TType): Name = tt match {
    case ttcons(n, _, _) => n
  }

  def appendTTypes(tt1: TType, tt2: TType): TType = (tt1, tt2) match {
    case (ttempty(), tt) => tt
    case (ttcons(name, ft, tttail1), tt) => ttcons(name, ft, appendTTypes(tttail1, tt))
  }

  // Value for a field (underspecified)
  trait Val extends Expression

  // table row, list of field values (with at least one cell/field per construction!)
  sealed trait Row extends Expression

  case class rempty() extends Row

  case class rcons(v: Val, r: Row) extends Row

  def rowLength(r: Row): nat = r match {
    case rempty() => zero()
    case rcons(_, rtail) => succ(rowLength(rtail))
  }

  // table matrix (list of rows), without "header" (attribute list)
  sealed trait RawTable extends Expression

  case class tempty() extends RawTable

  case class tcons(r: Row, rt: RawTable) extends RawTable

  def rawTableLength(rt: RawTable): nat = rt match {
    case tempty() => zero()
    case tcons(_, rttail) => succ(rawTableLength(rttail))
  }

  def onlyRowsWithSingleColumn(rt: RawTable): Boolean = rt match {
    case tempty() => true
    case tcons(rcons(_, rempty()), rttail) => onlyRowsWithSingleColumn(rttail)
    case _ => false
  }

  def onlyNonemptyRows(rt: RawTable): Boolean = rt match {
    case tempty() => true
    case tcons(rcons(_, _), rttail) => onlyNonemptyRows(rttail)
    case _ => false
  }

  def onlyRowsMatchingAttrLength(rt: RawTable, al: AttrL): Boolean = rt match {
    case tempty() => true
    case tcons(row, rttail) => rowLength(row) == attrListLength(al) && onlyRowsMatchingAttrLength(rttail, al)
  }

  // this will only work if both tables have the same row count
  def onlyRowsWithDecrementedLength(decremented: RawTable, original: RawTable): Boolean = (decremented, original) match {
    case (tempty(), tempty()) => true
    case (tcons(r1, rt1), tcons(r2, rt2)) =>
      succ(rowLength(r1)) == rowLength(r2) && onlyRowsWithDecrementedLength(rt1, rt2)
    case _ => false
  }

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
    case (tempty(), _) => tempty()
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

  case class bindStore(n: Name, t: Table, rst: TStore) extends TStore

  def lookupStore(n: Name, tst: TStore): OptTable = (n, tst) match {
    case (_, emptyStore()) => noTable()
    case (n1, bindStore(m, t, tsr)) =>
      if (n1 == m)
        someTable(t)
      else lookupStore(n1, tsr)
  }

  sealed trait TTContext extends Context

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
    case (Union(tvalue(table(al1, rt1)), tvalue(table(al2, rt2))), ts) =>
      someQuery(tvalue(table(al1, rawUnion(rt1, rt2))))
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
    case (Intersection(tvalue(table(al1, rt1)), tvalue(table(al2, rt2))), ts) =>
      someQuery(tvalue(table(al1, rawIntersection(rt1, rt2))))
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
    case (Difference(tvalue(table(al1, rt1)), tvalue(table(al2, rt2))), ts) =>
      someQuery(tvalue(table(al1, rawDifference(rt1, rt2))))
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

  def findColType(n: Name, tt: TType): OptFType = (n, tt) match {
    case (an, ttempty()) => noFType()
    case (an, ttcons(a, ft, ttr)) =>
      if (an == a)
        someFType(ft)
      else
        findColType(an, ttr)
  }

  def projectTypeAttrL(attrl: AttrL, tt: TType): OptTType = (attrl, tt) match {
    case (aempty(), tt1) => someTType(ttempty())
    case (acons(a, alr), tt1) =>
      val ft = findColType(a, tt1)
      val tprest = projectTypeAttrL(alr, tt1)
      if (isSomeFType(ft) && isSomeTType(tprest))
        someTType(ttcons(a, getFType(ft), getTType(tprest)))
      else
        noTType()
  }

  def projectType(sel: Select, tt: TType): OptTType = (sel, tt) match {
    case (all(), tt1) => someTType(tt1)
    case (list(al), tt1) => projectTypeAttrL(al, tt1)
  }

  def typeOfExp(e: Exp, tt: TType): OptFType = (e, tt) match {
    case (constant(fv), tt1) => someFType(fieldType(fv))
    case (lookup(a), ttempty()) => noFType()
    case (lookup(a), ttcons(a2, ft, ttr)) =>
      if (a == a2)
        someFType(ft)
      else
        typeOfExp(lookup(a), ttr)
  }


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
  def TTTContextDublicate(x: Name, y: Name, Tx: TType, Ty: TType, C: TTContext, e: Query, T: TType): Unit = {
    require(x == y)
    require(bindContext(x, Tx, bindContext(y, Ty, C)) |- e :: T)
  } ensuring (bindContext(x, Tx, C) |- e :: T)

  @Axiom
  def TTTContextSwap(x: Name, y: Name, Tx: TType, Ty: TType, C: TTContext, e: Query, T: TType): Unit = {
    require(x != y)
    require(bindContext(x, Tx, bindContext(y, Ty, C)) |- e :: T)
  } ensuring(bindContext(y, Ty, bindContext(x, Tx, C)) |- e :: T)

  @Axiom
  def Ttvalue(tt: TType, al: AttrL, rt: RawTable, TTC: TTContext, TT: TType): Unit = {
    require(welltypedtable(tt, table(al, rt)))
  } ensuring(TTC |- tvalue(table(al, rt)) :: TT)

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

  // type inversion axiom needed or not?

  // determines whether a given TTContext is consistent with a given TStore
  // and whether the table in the store is well-typed with regard to the table type in the context
  // design decision: require bindings to appear in exactly the SAME ORDER! (simpler?)
  def storeContextConsistent(ts: TStore, ttc: TTContext): Boolean = (ts, ttc) match {
    case (emptyStore(), emptyContext()) => true
    case (bindStore(tn1, t, tsr), bindContext(tn2, tt, ttcr)) =>
      tn1 == tn2 && welltypedtable(tt, t) && storeContextConsistent(tsr, ttcr)
    case (_, _) => false
  }

  //PROGRESS
  @Property
  def reduceProgress(ts: TStore, ttc: TTContext, q: Query): Unit = {
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
  def filterPreservesType(tt: TType, t: Table, p: Pred): Unit = {
    require(welltypedtable(tt, t))
  } ensuring welltypedtable(tt, filterTable(t, p))

  @Property
  def projectTableProgress(tt: TType, t: Table, s: Select, tt2: TType): Unit = {
    require(welltypedtable(tt, t))
    require(projectType(s, tt) == someTType(tt2))
  } ensuring exists((t2: Table) => projectTable(s, t) == someTable(t2))

  @Property
  def filterRowsPreservesTable(tt: TType, rt: RawTable, al: AttrL, p: Pred): Unit = {
    require(welltypedRawtable(tt, rt))
  } ensuring(welltypedRawtable(tt, filterRows(rt, al, p)))

  @Property
  def projectColsProgress(tt: TType, al: AttrL, rt: RawTable, al2: AttrL, tt2: TType): Unit = {
    require(welltypedtable(tt, table(al, rt)))
    require(projectType(list(al2), tt) == someTType(tt2))
  } ensuring exists((rt2: RawTable) => projectCols(al2, al, rt) == someRawTable(rt2))

  /* NOTE: Comparing to the corresponding Dafny lemma, we have moved the universal quantification
         from the conclusion to the premise. */
  @Property
  def projectTypeImpliesFindCol(tt: TType, al: AttrL, rt: RawTable, al2: AttrL, tt2: TType, n: Name): Unit = {
    require(welltypedtable(tt, table(al, rt)))
    require(projectTypeAttrL(al2, tt) == someTType(tt2))
    attrIn(n, al2)
  } ensuring exists((rt2: RawTable) => findCol(n, al, rt) == someRawTable(rt2))

  @Property
  def findColTypeImpliesfindCol(tt: TType, al: AttrL, rt: RawTable, n: Name, ft: FType): Unit = {
    require(welltypedtable(tt, table(al, rt)))
    require(findColType(n, tt) == someFType(ft))
  } ensuring exists((rt2: RawTable) => findCol(n, al, rt) == someRawTable(rt2))

  /* NOTE: Comparing to the corresponding Dafny lemma, we have moved the universal quantification
           from the conclusion to the premise. */
  @Property
  def projectTypeAttrLImpliesfindAllColType(al: AttrL, tt: TType, tt2: TType, n: Name): Unit = {
    require(projectTypeAttrL(al, tt) == someTType(tt2))
    require(attrIn(n, al))
  } ensuring exists((ft: FType) => findColType(n, tt) == someFType(ft))

  @Property
  def dropFirstColRawPreservesWelltypedRaw(tt: TType, n: Name, ft: FType, ttr: TType, rt: RawTable): Unit = {
    require(tt == ttcons(n, ft, ttr)) // |tt| > 0
    require(welltypedRawtable(tt, rt))
  } ensuring(welltypedRawtable(ttr, dropFirstColRaw(rt)))

  //PRESERVATION

  // union, intersection, difference preserve well-typedness of raw tables
  @Property
  def rawUnionPreservesWellTypedRaw(rt1: RawTable, rt2: RawTable, tt: TType): Unit = {
    require(welltypedRawtable(tt, rt1))
    require(welltypedRawtable(tt, rt2))
  } ensuring welltypedRawtable(tt, rawUnion(rt1, rt2))

  @Property
  def rawIntersectionPreservesWellTypedRaw(rt1: RawTable, rt2: RawTable, tt: TType): Unit = {
    require(welltypedRawtable(tt, rt1))
    require(welltypedRawtable(tt, rt2))
  } ensuring welltypedRawtable(tt, rawIntersection(rt1, rt2))

  @Property
  def rawDifferencePreservesWellTypedRaw(rt1: RawTable, rt2: RawTable, tt: TType): Unit = {
    require(welltypedRawtable(tt, rt1))
    require(welltypedRawtable(tt, rt2))
  } ensuring welltypedRawtable(tt, rawDifference(rt1, rt2))

  @Property
  def projectTypeAttrLMatchesProjectTableAttrL(al: AttrL, tt: TType, tt2: TType, t: Table, t2: Table): Unit = {
    require(welltypedtable(tt, t))
    require(projectTypeAttrL(al, tt) == someTType(tt2))
    require(projectTable(list(al), t) == someTable(t2))
  } ensuring matchingAttrL(tt2, getAttrL(t2))

  @Property
  def welltypedEmptyProjection(rt: RawTable): Unit = {
  } ensuring welltypedRawtable(ttempty(), projectEmptyCol(rt))

  @Property
  def projectFirstRawPreservesWelltypedRaw(a: Name, t: Table, tt: TType, tt2: TType, ft: FType, tt3: TType): Unit = {
    require(welltypedRawtable(tt, getRaw(t)))
    require(projectTypeAttrL(acons(a, aempty()), tt) == someTType(tt2))
    require(tt == ttcons(a, ft, tt3))
  } ensuring welltypedRawtable(tt2, projectFirstRaw(getRaw(t)))

  @Property
  def projectTypeFindCol(a: Name, t: table, tt: TType, tt2: TType, rt: RawTable): Unit = {
    require(welltypedtable(tt, t))
    require(projectTypeAttrL(acons(a, aempty()), tt) == someTType(tt2))
    require(findCol(a, getAttrL(t), getRaw(t)) == someRawTable(rt))
  } ensuring welltypedRawtable(tt2, rt)

  @Property
  def attachColToFrontRawPreservesWellTypedRaw(tt1: TType, name1: Name, ft1: FType,
                                               tt2: TType,
                                               rt1: RawTable, rt2: RawTable): Unit = {
    // |tt1| == 1
    require(tt1 == ttcons(name1, ft1, ttempty()))
    require(rawTableLength(rt1) == rawTableLength(rt2))
    require(welltypedRawtable(tt1, rt1))
    require(welltypedRawtable(tt1, rt2))
  } ensuring welltypedRawtable(appendTTypes(tt1, tt2), attachColToFrontRaw(rt1, rt2)) ensuring // TODO: how to wrap this?
    rawTableLength(rt1) == rawTableLength(attachColToFrontRaw(rt1, rt2)) // TODO: isn't this the lemma below?

  @Property
  def attachColToFrontRawPreservesRowCount(rt1: RawTable, rt2: RawTable): Unit = {
    require(rawTableLength(rt1) == rawTableLength(rt2))
    onlyRowsWithSingleColumn(rt1)
  } ensuring rawTableLength(rt1) == rawTableLength(attachColToFrontRaw(rt1, rt2))

  @Property
  def projectFirstRawPreservesRowCount(rt: RawTable): Unit = {
  } ensuring rawTableLength(rt) == rawTableLength(projectFirstRaw(rt))

  @Property
  def dropFirstColRawPreservesRowCount(rt: RawTable): Unit = {
  } ensuring rawTableLength(rt) == rawTableLength(dropFirstColRaw(rt))

  @Property
  def findColPreservesRowCount(a: Name, al: AttrL, rt: RawTable, rt2: RawTable): Unit = {
    require(findCol(a, al, rt) == someRawTable(rt2))
  } ensuring rawTableLength(rt) == rawTableLength(rt2)

  @Property
  def projectFirstRawYieldsSingleColumn(rt: RawTable): Unit = {
    require(onlyNonemptyRows(rt))
  } ensuring onlyRowsWithSingleColumn(projectFirstRaw(rt))

  @Property
  def dropFirstColRawDecrementsColCount(rt: RawTable): Unit = {
    require(rawTableLength(rt) == rawTableLength(dropFirstColRaw(rt)))
    require(onlyNonemptyRows(rt))
  } ensuring onlyRowsWithDecrementedLength(dropFirstColRaw(rt), rt)

  @Property
  def findColYieldsSingleColumn(a: Name, al: AttrL, rt: RawTable, rt2: RawTable): Unit = {
    require(findCol(a, al, rt) == someRawTable(rt2))
    require(onlyRowsMatchingAttrLength(rt, al))
  } ensuring onlyRowsWithSingleColumn(rt2)

  @Property
  def projectEmptyColPreservesRowCount(rt: RawTable): Unit = {
  } ensuring rawTableLength(rt) == rawTableLength(projectEmptyCol(rt))

  @Property
  def projectColsPreservesRowCount(al1: AttrL, al2: AttrL, rt: RawTable, rt2: RawTable): Unit = {
    require(projectCols(al1, al2, rt) == someRawTable(rt2))
    require(onlyRowsMatchingAttrLength(rt, al2))
  } ensuring rawTableLength(rt) == rawTableLength(rt2)

  @Property
  def projectColsWelltypedWithSelectType(al: AttrL, t: Table, tt: TType, tt2: TType, rt: RawTable): Unit = {
    require(welltypedtable(tt, t))
    require(projectTypeAttrL(al, tt) == someTType(tt2))
    require(projectCols(al, getAttrL(t), getRaw(t)) == someRawTable(rt))
  } ensuring welltypedRawtable(tt2, rt)

  @Property
  def projectTableWelltypedWithSelectType(sel: Select, t: Table, tt: TType, tt2: TType): Unit = {
    require(welltypedtable(tt, t))
    require(projectType(sel, tt) == someTType(tt2))
  } ensuring exists((t2: Table) => projectTable(sel, t) == someTable(t2)) ensuring welltypedtable(
    tt2, getTable(projectTable(sel, t))) // TODO: how to wrap this?

  @Property
  def reducePreservation(ttc: TTContext, ts: TStore, q: Query, qr: Query, tt: TType): Unit = {
    require(storeContextConsistent(ts, ttc))
    require(ttc |- q :: tt)
    require(reduce(q, ts) == someQuery(qr))
  } ensuring(ttc |- qr :: tt)

}
