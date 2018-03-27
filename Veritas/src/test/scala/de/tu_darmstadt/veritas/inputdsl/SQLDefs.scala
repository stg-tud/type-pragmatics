package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.ast._

/**
  * Created by cygne on 06.10.16.
  */
object SQLDefs {

  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._
  import de.tu_darmstadt.veritas.inputdsl.ProofDSL._
  import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._

  //module Table

  // name of attributes and tables
  val Name = open data 'Name

  // list of attribute names
  val AttrL = data('AttrL) of
    'aempty | 'acons ('Name, 'AttrL)

  val append: Functions = function('append.>>('AttrL, 'AttrL) -> 'AttrL) where
    ('append ('aempty, 'y) := 'y) |
      ('append ('acons ('a, 'al), 'y) := 'acons ('a, 'append ('al, 'y)))


  val FType = open data 'FType

  // type of a table (table schema)
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


  val getRaw: Functions = function('getRaw.>>('Table) -> 'RawTable) where
    ('getRaw ('table ('al, 'rt)) := 'rt)

  val getAttrL = function('getAttrL.>>('Table) -> 'AttrL) where
    ('getAttrL ('table ('al, 'rt)) := 'al)


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
  val matchingAttrL = function('matchingAttrL.>>('TType, 'AttrL) -> 'Bool) where
    ('matchingAttrL ('ttempty, 'aempty) := true) |
      ('matchingAttrL ('ttcons ('a1, 'f, 'tt), 'acons ('a2, 'al)) := ('a1 === 'a2) && 'matchingAttrL ('tt, 'al)) |
      ('matchingAttrL ('tt, 'al) := false)

  val welltypedtable = function('welltypedtable.>>('TType, 'Table) -> 'Bool) where
    ('welltypedtable ('tt, 'table ('al, 't)) := ('matchingAttrL ('tt, 'al) && 'welltypedRawtable ('tt, 't)))

  val welltypedRawtable = function('welltypedRawtable.>>('TType, 'RawTable) -> 'Bool) where
    ('welltypedRawtable ('tt, 'tempty) := true) |
      ('welltypedRawtable ('tt, 'tcons ('r, 't)) := 'welltypedRow ('tt, 'r) && 'welltypedRawtable ('tt, 't))

  val welltypedRow = function('welltypedRow.>>('TType, 'Row) -> 'Bool) where
    ('welltypedRow ('ttempty, 'rempty) := true) |
      ('welltypedRow ('ttcons ('a, 'ft, 'tt), 'rcons ('v, 'r)) := ('fieldType ('v) === 'ft) && 'welltypedRow ('tt, 'r)) |
      ('welltypedRow ('tt, 'r) := false)

  val Tables = Module("Tables", Seq(),
    Seq(Name, AttrL, append, FType, TType, Val,
      Row, RawTable, Table, getRaw, getAttrL,
      fieldType, lessThan, greaterThan, matchingAttrL,
      welltypedtable, welltypedRawtable, welltypedRow))

  //module TableAux

  //some auxiliary functions on raw tables (all not knowing anything about table types!)
  //the functions are intended to be used with well-typed tables!!

  val attrIn = function('attrIn.>>('Name, 'AttrL) -> 'Bool) where
    ('attrIn ('n, 'aempty) := false) |
      ('attrIn ('n, 'acons ('m, 'al)) := ('n === 'm) || 'attrIn ('n, 'al))


  val rowIn = function('rowIn.>>('Row, 'RawTable) -> 'Bool) where
    ('rowIn ('r1, 'tempty) := false) |
      ('rowIn ('r1, 'tcons ('r2, 'rt)) := ('r1 === 'r2) || 'rowIn ('r1, 'rt))


  //projects a raw table to its first column
  //returns a raw table with exactly one column or tempty
  val projectFirstRaw = function('projectFirstRaw.>>('RawTable) -> 'RawTable) where
    ('projectFirstRaw ('tempty) := 'tempty) |
      ('projectFirstRaw ('tcons ('rempty, 'rt)) := 'tcons ('rempty, 'projectFirstRaw ('rt))) |
      ('projectFirstRaw ('tcons ('rcons ('f, 'r), 'rt)) := 'tcons ('rcons ('f, 'rempty), 'projectFirstRaw ('rt)))


  //drops the first column of a raw table
  //returns a raw table with one column less than before or tempty
  val dropFirstColRaw = function('dropFirstColRaw.>>('RawTable) -> 'RawTable) where
    ('dropFirstColRaw ('tempty) := 'tempty) |
      ('dropFirstColRaw ('tcons ('rempty, 'rt)) := 'tcons ('rempty, 'dropFirstColRaw ('rt))) |
      ('dropFirstColRaw ('tcons ('rcons ('f, 'r), 'rt)) := 'tcons ('r, 'dropFirstColRaw ('rt)))

  val OptRawTable = data('OptRawTable) of
    'noRawTable | 'someRawTable ('RawTable)

  val isSomeRawTable = function('isSomeRawTable.>>('OptRawTable) -> 'Bool) where
    ('isSomeRawTable ('noRawTable) := false) |
      ('isSomeRawTable ('someRawTable ('t)) := true)

  val getRawTable = partial(function('getRawTable.>>('OptRawTable) -> 'RawTable) where
    ('getRawTable ('someRawTable ('t)) := 't))

  //attaches a raw table with one column to the front of another raw table
  //returns a raw table with one column more
  //blindly assumes that both tables have the same row count!
  //fails if input tables do not have the same row count or if the first
  //include empty brackets after tempty such that the parser does not report an error
  //is treated exactly like tempty for fof-generation
  // function
  // attachColToFrontRaw : RawTable RawTable -> OptRawTable // Opt[RawTable]
  // attachColToFrontRaw(tempty(), tempty()) = someRawTable(tempty)
  // attachColToFrontRaw(tcons(rcons(f, rempty), rt1), tcons(r, rt2)) =
  // 	let rest = attachColToFrontRaw(rt1, rt2) in
  // 		if (isSomeRawTable(rest))
  // 		then someRawTable(tcons(rcons(f, r), getRawTable(rest)))
  // 		else noRawTable
  // attachColToFrontRaw(rt1, rt2) = noRawTable

  //attaches a raw table with one column to the front of another raw table
  //returns a raw table with one column more, possibly not a welltyped one
  //(if the row counts of the input arguments differ)
  //assumes that both tables have the same row count!
  //include empty brackets after tempty such that the parser does not report an error
  //is treated exactly like tempty for fof-generation
  val attachColToFrontRaw = function('attachColToFrontRaw.>>('RawTable, 'RawTable) -> 'RawTable) where
    ('attachColToFrontRaw ('tempty, 'tempty) := 'tempty) |
      ('attachColToFrontRaw ('tcons ('rcons ('f, 'rempty), 'rt1), 'tcons ('r, 'rt2)) :=
        'tcons ('rcons ('f, 'r), 'attachColToFrontRaw ('rt1, 'rt2))) |
      ('attachColToFrontRaw ('rt1, 'rt2) := 'tcons ('rempty, 'tempty))


  //definition: union removes duplicate rows
  //(but only between the two tables, not within a table!)
  //preserves row order of the two original raw tables
  val rawUnion = function('rawUnion.>>('RawTable, 'RawTable) -> 'RawTable) where
    ('rawUnion ('tempty, 'rt2) := 'rt2) |
      ('rawUnion ('rt1, 'tempty) := 'rt1) |
      ('rawUnion ('tcons ('r1, 'rt1), 'rt2) :=
        (let('urt1rt2) := 'rawUnion ('rt1, 'rt2)) in
          (iff(!'rowIn ('r1, 'rt2))
            th 'tcons ('r1, 'urt1rt2)
            els 'urt1rt2))

  //preserves order of rows in first argument
  val rawIntersection = function('rawIntersection.>>('RawTable, 'RawTable) -> 'RawTable) where
    ('rawIntersection ('tempty, 'rt2) := 'tempty) |
      ('rawIntersection ('rt1, 'tempty) := 'tempty) |
      ('rawIntersection ('tcons ('r1, 'tempty), 'rt2) :=
        (iff('rowIn ('r1, 'rt2))
          th 'tcons ('r1, 'tempty)
          els 'tempty)) |
      ('rawIntersection ('tcons ('r1, 'rt1), 'rt2) :=
        (let('urt1rt2) := 'rawIntersection ('rt1, 'rt2)) in
          (iff('rowIn ('r1, 'rt2))
            th 'tcons ('r1, 'urt1rt2)
            els 'urt1rt2))

  val rawDifference = function('rawDifference.>>('RawTable, 'RawTable) -> 'RawTable) where
    ('rawDifference ('tempty, 'rt2) := 'tempty) |
      ('rawDifference ('rt1, 'tempty) := 'rt1) |
      ('rawDifference ('tcons ('r1, 'tempty), 'rt2) :=
        (iff(!'rowIn ('r1, 'rt2))
          th 'tcons ('r1, 'tempty)
          els 'tempty)) |
      ('rawDifference ('tcons ('r1, 'rt1), 'rt2) :=
        (let('drt1rt2) := 'rawDifference ('rt1, 'rt2)) in
          (iff(!'rowIn ('r1, 'rt2))
            th 'tcons ('r1, 'drt1rt2)
            els 'drt1rt2))

  val TableAux = Module("TableAux", Seq(Resolved(Tables)),
    Seq(attrIn, rowIn, projectFirstRaw,
      dropFirstColRaw, OptRawTable,
      isSomeRawTable, getRawTable, attachColToFrontRaw,
      rawUnion, rawIntersection, rawDifference))

  //module sql.TStore

  val OptTable = data('OptTable) of
    'noTable | 'someTable ('Table)

  val isSomeTable = function('isSomeTable.>>('OptTable) -> 'Bool) where
    ('isSomeTable ('noTable) := false) |
      ('isSomeTable ('someTable ('t)) := true)

  val getTable = partial(function('getTable.>>('OptTable) -> 'Table) where
    ('getTable ('someTable ('t)) := 't))

  val TStoreData: DataType = data('TStore) of
    'emptyStore | 'bindStore ('Name, 'Table, 'TStore)

  val lookupStore = function('lookupStore.>>('Name, 'TStore) -> 'OptTable) where
    ('lookupStore ('n, 'emptyStore) := 'noTable) |
      ('lookupStore ('n, 'bindStore ('m, 't, 'TS)) :=
        (iff('n === 'm)
          th 'someTable ('t)
          els 'lookupStore ('n, 'TS)))

  val TStore = Module("TStore", Seq(Resolved(Tables)),
    Seq(OptTable, isSomeTable, getTable, TStoreData, lookupStore))

  //module sql.TContext

  //import sql.Tables
  //import sql.TStore

  val TTContext = data('TTContext) of
    'emptyContext |
      'bindContext ('Name, 'TType, 'TTContext)

  val OptTType = data('OptTType) of
    'noTType |
      'someTType ('TType)

  val isSomeTType = function('isSomeTType.>>('OptTType) -> 'Bool) where
    ('isSomeTType ('noTType) := false) |
      ('isSomeTType ('someTType ('t)) := true)

  val getTType = partial(function('getTType.>>('OptTType) -> 'TType) where
    ('getTType ('someTType ('t)) := 't))


  val lookupContext = function('lookupContext.>>('Name, 'TTContext) -> 'OptTType) where
    ('lookupContext ('tn, 'emptyContext) := 'noTType) |
      ('lookupContext ('tn, 'bindContext ('tm, 'tt, 'TTC)) :=
        (iff('tn === 'tm)
          th 'someTType ('tt)
          els 'lookupContext ('tn, 'TTC)))

  val TContext = Module("TContext", Seq(Resolved(Tables), Resolved(TStore)),
    Seq(TTContext, OptTType, isSomeTType, getTType, lookupContext))

  //module sql.Syntax

  //import sql.Tables
  //import sql.TStore

  val Exp = data('Exp) of 'constant ('Val) | 'lookup ('Name)

  //predicates for where clauses of queries
  val Pred = data('Pred) of
    'ptrue |
      'and ('Pred, 'Pred) |
      'not ('Pred) |
      'eq ('Exp, 'Exp) |
      'gt ('Exp, 'Exp) |
      'lt ('Exp, 'Exp)


  // Query syntax
  val Select = data('Select) of 'all | 'list ('AttrL)

  val Query = data('Query) of
    'tvalue ('Table) |
      'selectFromWhere ('Select, 'Name, 'Pred) |
      'Union ('Query, 'Query) |
      'Intersection ('Query, 'Query) |
      'Difference ('Query, 'Query)

  val isValue = function('isValue.>>('Query) -> 'Bool) where
    ('isValue ('tvalue ('t)) := true) |
      ('isValue ('selectFromWhere ('s, 't, 'p)) := false) |
      ('isValue ('Union ('q1, 'q2)) := false) |
      ('isValue ('Intersection ('sql1, 'sql2)) := false) |
      ('isValue ('Difference ('sql1, 'sql2)) := false)

  val Syntax = Module("Syntax", Seq(Resolved(Tables), Resolved(TStore)),
    Seq(Exp, Pred, Select, Query, isValue))

  //module sql.Semantics

  //import sql.Tables
  //import sql.TableAux
  //import sql.Syntax
  //import sql.TStore


  val OptQuery = data('OptQuery) of
    'noQuery |
      'someQuery ('Query)

  val isSomeQuery: Functions = function('isSomeQuery.>>('OptQuery) -> 'Bool)
  ('isSomeQuery ('noQuery) := false) |
    ('isSomeQuery ('someQuery ('q)) := true)

  val getQuery = partial(function('getQuery.>>('OptQuery) -> 'Query) where
    ('getQuery ('someQuery ('q)) := 'q))


  val findCol = function('findCol.>>('Name, 'AttrL, 'RawTable) -> 'OptRawTable) where
    ('findCol ('a, 'aempty, 'rt) := 'noRawTable) |
      ('findCol ('a, 'acons ('a2, 'al), 'rt) :=
        (iff('a === 'a2)
          th 'someRawTable ('projectFirstRaw ('rt))
          els 'findCol ('a, 'al, 'dropFirstColRaw ('rt))))

  // for projection base case: projecting on an empty attribute list must yield a
  // table with as many empty rows as the rowcount of the given table
  val projectEmptyCol = function('projectEmptyCol.>>('RawTable) -> 'RawTable) where
    ('projectEmptyCol ('tempty) := 'tempty) |
      ('projectEmptyCol ('tcons ('r, 't)) := 'tcons ('rempty, 'projectEmptyCol ('t)))

  // select-list table-list table-rows
  val projectCols = function('projectCols.>>('AttrL, 'AttrL, 'RawTable) -> 'OptRawTable) where
    ('projectCols ('aempty, 'al, 'rt) := 'someRawTable ('projectEmptyCol ('rt))) |
      ('projectCols ('acons ('a, 'alr), 'al, 'rt) :=
        (let('col) := 'findCol ('a, 'al, 'rt)) in
          ((let('rest) := 'projectCols ('alr, 'al, 'rt)) in
            (iff('isSomeRawTable ('col) && 'isSomeRawTable ('rest))
              th 'someRawTable ('attachColToFrontRaw ('getRawTable ('col), 'getRawTable ('rest)))
              els 'noRawTable)))

  val projectTable = function('projectTable.>>('Select, 'Table) -> 'OptTable) where
    ('projectTable ('all, 'table ('al, 'rt)) := 'someTable ('table ('al, 'rt))) |
      ('projectTable ('list ('alr), 'table ('al, 'rt)) :=
        (let('projected) := 'projectCols ('alr, 'al, 'rt)) in
          (iff('isSomeRawTable ('projected))
            th 'someTable ('table ('alr, 'getRawTable ('projected)))
            els 'noTable))


  val OptVal = data('OptVal) of 'noVal | 'someVal ('Val)
  val OptRow = data('OptRow) of 'noRow | 'someRow ('Row)

  val isSomeVal = function('isSomeVal.>>('OptVal) -> 'Bool) where
    ('isSomeVal ('noVal) := false) |
      ('isSomeVal ('someVal ('v)) := true)

  val getVal = partial(function('getVal.>>('OptVal) -> 'Val) where
    ('getVal ('someVal ('v)) := 'v))

  val evalExpRow = function('evalExpRow.>>('Exp, 'AttrL, 'Row) -> 'OptVal) where
    ('evalExpRow ('constant ('v), 'al, 'r) := 'someVal ('v)) |
      ('evalExpRow ('lookup ('a), 'acons ('a2, 'al), 'rcons ('v, 'r)) :=
        (iff('a === 'a2)
          th 'someVal ('v)
          els 'evalExpRow ('lookup ('a), 'al, 'r))) |
      ('evalExpRow ('e, 'al, 'r) := 'noVal)

  // returns true iff predicate succeeds on row
  // returns false if predicate evaluates to false or if predicate evaluation fails
  val filterSingleRow = function('filterSingleRow.>>('Pred, 'AttrL, 'Row) -> 'Bool) where
    ('filterSingleRow ('ptrue, 'al, 'r) := true) |
      ('filterSingleRow ('and ('p1, 'p2), 'al, 'r) :=
        ('filterSingleRow ('p1, 'al, 'r) && 'filterSingleRow ('p2, 'al, 'r))) |
      ('filterSingleRow ('not ('p), 'al, 'r) := (!'filterSingleRow ('p, 'al, 'r))) |
      ('filterSingleRow ('eq ('e1, 'e2), 'al, 'r) :=
        (let('v1) := 'evalExpRow ('e1, 'al, 'r)) in
          ((let('v2) := 'evalExpRow ('e2, 'al, 'r)) in
            ('isSomeVal ('v1) && 'isSomeVal ('v2) && ('getVal ('v1) === 'getVal ('v2))))) |
      ('filterSingleRow ('gt ('e1, 'e2), 'al, 'r) :=
        (let('v1) := 'evalExpRow ('e1, 'al, 'r)) in
          ((let('v2) := 'evalExpRow ('e2, 'al, 'r)) in
            ('isSomeVal ('v1) && 'isSomeVal ('v2) && 'greaterThan ('getVal ('v1), 'getVal ('v2))))) |
      ('filterSingleRow ('lt ('e1, 'e2), 'al, 'r) :=
        (let('v1) := 'evalExpRow ('e1, 'al, 'r)) in
          ((let('v2) := 'evalExpRow ('e2, 'al, 'r)) in
            ('isSomeVal ('v1) && 'isSomeVal ('v2) && 'lessThan ('getVal ('v1), 'getVal ('v2)))))

  // filter rows that satisfy pred
  val filterRows = function('filterRows.>>('RawTable, 'AttrL, 'Pred) -> 'RawTable) where
    ('filterRows ('tempty, 'al, 'pred) := 'tempty) |
      ('filterRows ('tcons ('r, 'rt), 'al, 'pred) :=
        (let('rts) := 'filterRows ('rt, 'al, 'pred)) in
          (iff('filterSingleRow ('pred, 'al, 'r))
            th 'tcons ('r, 'rts)
            els 'rts))

  val filterTable = function('filterTable.>>('Table, 'Pred) -> 'Table) where
    ('filterTable ('table ('al, 'rt), 'p) := 'table ('al, 'filterRows ('rt, 'al, 'p)))


  // reduce fails if referenced tables are not found
  val reduce = function('reduce.>>('Query, 'TStore) -> 'OptQuery) where
    ('reduce ('tvalue ('t), 'ts) := 'noQuery) |
      ('reduce ('selectFromWhere ('sel, 'name, 'pred), 'ts) :=
        (let('maybeTable) := 'lookupStore ('name, 'ts)) in
          (iff('isSomeTable ('maybeTable))
            th ((let('filtered) := 'filterTable ('getTable ('maybeTable), 'pred)) in
            ((let('maybeSelected) := 'projectTable ('sel, 'filtered)) in
              (iff('isSomeTable ('maybeSelected))
                th 'someQuery ('tvalue ('getTable ('maybeSelected)))
                els 'noQuery)))
            els 'noQuery)) |
      ('reduce ('Union ('tvalue ('table ('al1, 'rt1)), 'tvalue ('table ('al2, 'rt2))), 'ts) :=
        'someQuery ('tvalue ('table ('al1, 'rawUnion ('rt1, 'rt2))))) |
      ('reduce ('Union ('tvalue ('t), 'q2), 'ts) :=
        (let('q2reduce) := 'reduce ('q2, 'ts)) in
          (iff('isSomeQuery ('q2reduce))
            th 'someQuery ('Union ('tvalue ('t), 'getQuery ('q2reduce)))
            els 'noQuery)) |
      ('reduce ('Union ('q1, 'q2), 'ts) :=
        (let('q1reduce) := 'reduce ('q1, 'ts)) in
          (iff('isSomeQuery ('q1reduce))
            th 'someQuery ('Union ('getQuery ('q1reduce), 'q2))
            els 'noQuery)) |
      ('reduce ('Intersection ('tvalue ('table ('al1, 'rt1)), 'tvalue ('table ('al2, 'rt2))), 'ts) :=
        'someQuery ('tvalue ('table ('al1, 'rawIntersection ('rt1, 'rt2))))) |
      ('reduce ('Intersection ('tvalue ('t), 'sql2), 'ts) :=
        (let('sql2reduce) := 'reduce ('sql2, 'ts)) in
          (iff('isSomeQuery ('sql2reduce))
            th 'someQuery ('Intersection ('tvalue ('t), 'getQuery ('sql2reduce)))
            els 'noQuery)) |
      ('reduce ('Intersection ('sql1, 'sql2), 'ts) :=
        (let('sql1reduce) := 'reduce ('sql1, 'ts)) in
          (iff('isSomeQuery ('sql1reduce))
            th 'someQuery ('Intersection ('getQuery ('sql1reduce), 'sql2))
            els 'noQuery)) |
      ('reduce ('Difference ('tvalue ('table ('al1, 'rt1)), 'tvalue ('table ('al2, 'rt2))), 'ts) :=
        'someQuery ('tvalue ('table ('al1, 'rawDifference ('rt1, 'rt2))))) |
      ('reduce ('Difference ('tvalue ('t), 'sql2), 'ts) :=
        (let('sql2reduce) := 'reduce ('sql2, 'ts)) in
          (iff('isSomeQuery ('sql2reduce))
            th 'someQuery ('Difference ('tvalue ('t), 'getQuery ('sql2reduce)))
            els 'noQuery)) |
      ('reduce ('Difference ('sql1, 'sql2), 'ts) :=
        (let('sql1reduce) := 'reduce ('sql1, 'ts)) in
          (iff('isSomeQuery ('sql1reduce))
            th 'someQuery ('Difference ('getQuery ('sql1reduce), 'sql2))
            els 'noQuery))

  val Semantics = Module("Semantics", Seq(Resolved(Tables), Resolved(TableAux), Resolved(Syntax), Resolved(TStore)),
    Seq(OptQuery, isSomeQuery, getQuery, findCol, projectEmptyCol, projectCols, projectTable,
      OptVal, OptRow, isSomeVal, getVal, evalExpRow, filterSingleRow, filterRows, filterTable,
      reduce))


  //module sql.TypeSystem

  //import sql.Tables
  //import sql.TStore
  //import sql.Syntax
  //import sql.TContext

  val OptFType = data('OptFType) of
    'noFType | 'someFType ('FType)

  val isSomeFType = function('isSomeFType.>>('OptFType) -> 'Bool) where
    ('isSomeFType ('noFType) := false) |
      ('isSomeFType ('someFType ('a)) := true)

  val getFType = partial(function('getFType.>>('OptFType) -> 'FType) where
    ('getFType ('someFType ('a)) := 'a)
  )

  val findColType = function('findColType.>>('Name, 'TType) -> 'OptFType) where
    ('findColType ('an, 'ttempty) := 'noFType) |
      ('findColType ('an, 'ttcons ('a, 'ft, 'ttr)) :=
        (iff('an === 'a)
          th 'someFType ('ft)
          els 'findColType ('an, 'ttr)))

  val projectType = function('projectType.>>('Select, 'TType) -> 'OptTType) where
    ('projectType ('all, 'tt) := 'someTType ('tt)) |
      ('projectType ('list ('al), 'tt) := 'projectTypeAttrL ('al, 'tt))

  val projectTypeAttrL = function('projectTypeAttrL.>>('AttrL, 'TType) -> 'OptTType) where
    ('projectTypeAttrL ('aempty, 'tt) := 'someTType ('ttempty)) |
      ('projectTypeAttrL ('acons ('a, 'alr), 'tt) :=
        (let('ft) := 'findColType ('a, 'tt)) in
          ((let('tprest) := 'projectTypeAttrL ('alr, 'tt)) in
            (iff('isSomeFType ('ft) && 'isSomeTType ('tprest))
              th 'someTType ('ttcons ('a, 'getFType ('ft), 'getTType ('tprest)))
              els 'noTType)))

  val typeOfExp = function('typeOfExp.>>('Exp, 'TType) -> 'OptFType) where
    ('typeOfExp ('constant ('fv), 'tt) := 'someFType ('fieldType ('fv))) |
      ('typeOfExp ('lookup ('a), 'ttempty) := 'noFType) |
      ('typeOfExp ('lookup ('a), 'ttcons ('a2, 'ft, 'tt)) :=
        (iff('a === 'a2)
          th 'someFType ('ft)
          els 'typeOfExp ('lookup ('a), 'tt)))

  val tcheckPred = function('tcheckPred.>>('Pred, 'TType) -> 'Bool) where
    ('tcheckPred ('ptrue, 'tt) := true) |
      ('tcheckPred ('and ('p1, 'p2), 'tt) := 'tcheckPred ('p1, 'tt) && 'tcheckPred ('p2, 'tt)) |
      ('tcheckPred ('not ('p), 'tt) := 'tcheckPred ('p, 'tt)) |
      ('tcheckPred ('eq ('e1, 'e2), 'tt) :=
        ((let('t1) := 'typeOfExp ('e1, 'tt)) in
          ((let('t2) := 'typeOfExp ('e2, 'tt)) in
            ('isSomeFType ('t1) && 'isSomeFType ('t2) && ('getFType ('t1) === 'getFType ('t2)))))) |
      ('tcheckPred ('gt ('e1, 'e2), 'tt) :=
        ((let('t1) := 'typeOfExp ('e1, 'tt)) in
          ((let('t2) := 'typeOfExp ('e2, 'tt)) in
            'isSomeFType ('t1) && 'isSomeFType ('t2) && ('getFType ('t1) === 'getFType ('t2))))) |
      ('tcheckPred ('lt ('e1, 'e2), 'tt) :=
        ((let('t1) := 'typeOfExp ('e1, 'tt)) in
          ((let('t2) := 'typeOfExp ('e2, 'tt)) in
            ('isSomeFType ('t1) && 'isSomeFType ('t2) && ('getFType ('t1) === 'getFType ('t2))))))


//  //axioms on behavior of table type context
  val TTTContextDuplicate: Axioms = axiom(
    ((~'x === ~'y) &
      ('bindContext (~'x, ~'Tx, 'bindContext (~'y, ~'Ty, ~'C)) |- ~'e :: ~'T)
      ).===>("T-TTContext-Duplicate")(
      'bindContext (~'x, ~'Tx, ~'C) |- ~'e :: ~'T
    ))

  val TTTContextSwap = axiom(
    ((~'x ~= ~'y) &
      ('bindContext (~'x, ~'Tx, 'bindContext (~'y, ~'Ty, ~'C)) |- ~'e :: ~'T)
      ).===>("T-TTContext-Swap")(
      'bindContext (~'y, ~'Ty, 'bindContext (~'x, ~'Tx, ~'C)) |- ~'e :: ~'T
    ))

  //a table value with a well-typed table is typable
  val Ttvalue = axiom(
    'welltypedtable (~'TT, 'table (~'al, ~'rt)
    ).===>("T-tvalue")(
      ~'TTC |- 'tvalue ('table (~'al, ~'rt)) :: ~'TT
    ))

  val TSelectFromWhere = axiom(
    (('lookupContext (~'tn, ~'TTC) === 'someTType (~'TT)) &
      'tcheckPred (~'p, ~'TT) &
      ('projectType (~'sel, ~'TT) === 'someTType (~'TTr))
      ).===>("T-SelectFromWhere")(
      ~'TTC |- 'selectFromWhere (~'sel, ~'tn, ~'p) :: ~'TTr
    )
  )

  val TUnion = axiom(
    ((~'TTC |- ~'q1 :: ~'TT) &
      (~'TTC |- ~'q2 :: ~'TT)
      ).===>("T-Union")(
      ~'TTC |- 'Union (~'q1, ~'q2) :: ~'TT
    ))

  val TIntersection = axiom(
    ((~'TTC |- ~'q1 :: ~'TT) &
      (~'TTC |- ~'q2 :: ~'TT)
      ).===>("T-Intersection")(
      ~'TTC |- 'Intersection (~'q1, ~'q2) :: ~'TT)
  )

  val TDifference = axiom(
    ((~'TTC |- ~'q1 :: ~'TT) &
      (~'TTC |- ~'q2 :: ~'TT)
      ).===>("T-Difference")(
      ~'TTC |- 'Difference (~'q1, ~'q2) :: ~'TT
    ))

  val TypeSystem = Module("TypeSystem", Seq(Resolved(Tables), Resolved(TStore), Resolved(Syntax), Resolved(TContext)),
    Seq(OptFType, isSomeFType, getFType, findColType, projectType, projectTypeAttrL,
      typeOfExp, tcheckPred, TTTContextDuplicate, TTTContextSwap, Ttvalue, TSelectFromWhere,
      TUnion, TIntersection, TDifference))
//
//  //module sql.TypeSystemInv
//
//  //import sql.Syntax
//  //import sql.TypeSystem
//  //import sql.Tables
//  //import sql.TContext
//
  val Tinv = axiom(
    (~'TTC |- ~'q :: ~'TT
      ).===>("T-inv")(
      OR(
        =>>(exists(~'al, ~'rt) |
          (~'q === 'tvalue ('table (~'al, ~'rt))) &
            'welltypedtable (~'TT, 'table (~'al, ~'rt))) |
          =>>(exists(~'sel, ~'tn, ~'p, ~'TTr) |
            (~'q === 'selectFromWhere (~'sel, ~'tn, ~'p)) &
              ('lookupContext (~'tn, ~'TTC) === 'someTType (~'TTr)) &
              'tcheckPred (~'p, ~'TTr) &
              ('projectType (~'sel, ~'TTr) === 'someTType (~'TT))) |
          =>>(exists(~'q1, ~'q2) |
            (~'q === 'Union (~'q1, ~'q2)) &
              (~'TTC |- ~'q1 :: ~'TT) &
              (~'TTC |- ~'q2 :: ~'TT)) |
          =>>(exists(~'q1, ~'q2) |
            (~'q === 'Intersection (~'q1, ~'q2)) &
              (~'TTC |- ~'q1 :: ~'TT) &
              (~'TTC |- ~'q2 :: ~'TT)) |
          =>>(exists(~'q1, ~'q2) |
            (~'q === 'Difference (~'q1, ~'q2)) &
              (~'TTC |- ~'q1 :: ~'TT) &
              (~'TTC |- ~'q2 :: ~'TT)))
    ))

  //variant of inversion lemma in several different lemmas
  //(seems to be helpful for progress proof)
  //not provable, but also not needed yet
  // lemma
  // ~q == tvalue(table(~al, ~rt))
  // ~TTC |- ~q : ~TT
  // ==================================================== T-inv-tvalue
  // welltypedtable(~TT, table(~al, ~rt))


  val TinvSelectSomeFromWhere = lemma(
    ((~'q === 'selectFromWhere (~'sel, ~'tn, ~'p)) &
      (~'TTC |- ~'q :: ~'TT)
      ).===>("T-inv-SelectSomeFromWhere")(
      exists(~'TTr) |
        ('lookupContext (~'tn, ~'TTC) === 'someTType (~'TTr)) &
          ('tcheckPred (~'p, ~'TTr)) &
          ('projectType (~'sel, ~'TTr) === 'someTType (~'TT)))
  )


  val TinvUnion = lemma(
    ((~'q === 'Union (~'q1, ~'q2)) &
      (~'TTC |- ~'q :: ~'TT)
      ).===>("T-inv-Union")(
      (~'TTC |- ~'q1 :: ~'TT) &
        (~'TTC |- ~'q2 :: ~'TT))
  )

  val TinvIntersection = lemma(
    ((~'q === 'Intersection (~'q1, ~'q2)) &
      (~'TTC |- ~'q :: ~'TT)
      ).===>("T-inv-Intersection")(
      (~'TTC |- ~'q1 :: ~'TT) &
        (~'TTC |- ~'q2 :: ~'TT))
  )

  val TinvDifference = lemma(
    ((~'q === 'Difference (~'q1, ~'q2)) &
      (~'TTC |- ~'q :: ~'TT)
      ).===>("T-inv-Difference")(
      (~'TTC |- ~'q1 :: ~'TT) &
        (~'TTC |- ~'q2 :: ~'TT))
  )

  val TypeSystemInv = Module("TypeSystemInv", Seq(Resolved(Syntax), Resolved(TypeSystem), Resolved(Tables), Resolved(TContext)),
    Seq(Tinv, TinvSelectSomeFromWhere, TinvUnion, TinvIntersection, TinvDifference))


  //module sql.SoundnessAuxDefs

  //import sql.Tables
  //import sql.TStore
  //import sql.Syntax
  //  import sql.TContext
  //  import sql.TypeSystem

  // determines whether a given TTContext is consistent with a given TStore
  // and whether the table in the store is well-typed with regard to the table type in the context
  // design decision: require bindings to appear in exactly the SAME ORDER! (simpler?)
  val StoreContextConsistent = function('StoreContextConsistent.>>('TStore, 'TTContext) -> 'Bool) where
    ('StoreContextConsistent ('emptyStore, 'emptyContext) := true) |
      ('StoreContextConsistent ('bindStore ('tn1, 't, 'tsr), 'bindContext ('tn2, 'tt, 'ttcr)) :=
        (('tn1 === 'tn2) && 'welltypedtable ('tt, 't)) && 'StoreContextConsistent ('tsr, 'ttcr)) |
      ('StoreContextConsistent ('ts, 'ttc) := false)

  val SoundnessAuxDefs = Module("SoundnessAuxDefs", Seq(Resolved(Tables), Resolved(TStore), Resolved(Syntax), Resolved(TContext), Resolved(TypeSystem)),
    Seq(StoreContextConsistent))


}
