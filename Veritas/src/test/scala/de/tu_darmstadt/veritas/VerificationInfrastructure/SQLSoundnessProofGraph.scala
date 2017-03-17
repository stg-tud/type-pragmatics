package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.{CaseDistinctionCase, StructInductCase, Tactic}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}
import org.scalatest.FunSuite

/**
  * Created by sylvia on 28/02/2017.
  */
class SQLSoundnessProofGraph extends FunSuite {

  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._
  import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._
  import de.tu_darmstadt.veritas.inputdsl.ProofDSL._

  import de.tu_darmstadt.veritas.inputdsl.SQLDefs._

  case class Spec(content: Seq[VeritasConstruct]) extends Ordered[Spec] {
    val ord = Ordering.Iterable[VeritasConstruct](Ordering.ordered[VeritasConstruct](x => x))

    override def compare(that: Spec): Int = ord.compare(this.content, that.content)
  }

  val fullSQLspec: Spec = Spec(Tables.defs ++ TableAux.defs ++ TStore.defs ++ TContext.defs ++
    Syntax.defs ++ Semantics.defs ++ TypeSystem.defs ++ TypeSystemInv.defs ++ SoundnessAuxDefs.defs)


  // We instantiate S = Spec and P = VeritasConstruct
  // When we construct a Transformer that reuses our previous transformations to TPTP, we
  // might have to explicitly construct Module(s).

  val file = File.createTempFile("sql-progress-proof-store", "")
  file.delete()
  file.mkdir()
  println(s"Test entity store: $file")

  val g: ProofGraphXodus[Spec, VeritasConstruct] =
    new ProofGraphXodus[Spec, VeritasConstruct](file)

  val progressObligation: g.Obligation = g.newObligation(fullSQLspec, SQLProgress)
  g.storeObligation("SQL progress", progressObligation)

  //Mock tactics
  // class for creating mock induction tactics, with convenience methods like selectCase
  class MockInduction(inductionvar: Spec) extends Tactic[Spec, VeritasConstruct] {

    override def apply[Obligation](obl: GenObligation[Spec, VeritasConstruct],
                                   produce: ObligationProducer[Spec, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] =
      Seq()

    override def compare(that: Tactic[Spec, VeritasConstruct]): Int = ???

  }

  object MockInduction {
    def selectCase[Obligation](name: String, required: Iterable[(Obligation, EdgeLabel)]): Obligation =
      required.find(_._2.asInstanceOf[StructInductCase[Spec, VeritasConstruct]].casename == name).get._1
  }


  //class for creating mock case distinctions
  class MockCaseDistinction(cases: Seq[Spec]) extends Tactic[Spec, VeritasConstruct] {

    override def apply[Obligation](obl: GenObligation[Spec, VeritasConstruct],
                                   produce: ObligationProducer[Spec, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] =
      Seq()

    override def compare(that: Tactic[Spec, VeritasConstruct]): Int = ???
  }

  object MockCaseDistinction {
    def selectCase[Obligation](name: String, required: Iterable[(Obligation, EdgeLabel)]): Obligation =
      required.find(_._2.asInstanceOf[CaseDistinctionCase[Spec, VeritasConstruct]].casename == name).get._1
  }


  // Apply structural induction to progress root via ad-hoc instance of MockInduction,
  // where the goals that are supposed to be generated are just hard-coded
  val rootInductionProgress =
  new MockInduction(Spec(Seq(MetaVar("q")))) {

    override def apply[Obligation](obl: GenObligation[Spec, VeritasConstruct], produce: ObligationProducer[Spec, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val tvaluecase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTtvalue)

      val selectfromwherecase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTselectFromWhere)

      val unioncase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTUnion)

      val intersectioncase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTIntersection)

      val differencecase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTDifference)

      Seq((tvaluecase, StructInductCase[Spec, VeritasConstruct](SQLProgressTtvalue.goals.head.name, None, Seq())),
        (selectfromwherecase, StructInductCase[Spec, VeritasConstruct](SQLProgressTselectFromWhere.goals.head.name, None, Seq())),
        (unioncase, StructInductCase[Spec, VeritasConstruct](SQLProgressTUnion.goals.head.name, Some(Spec(Seq(unionconsts))),
          Seq(SQLProgressTUnionIH1, SQLProgressTUnionIH2))),
        (intersectioncase, StructInductCase[Spec, VeritasConstruct](SQLProgressTIntersection.goals.head.name, Some(Spec(Seq(intersectionconsts))),
          Seq(SQLProgressTIntersectionIH1, SQLProgressTIntersectionIH2))),
        (differencecase, StructInductCase[Spec, VeritasConstruct](SQLProgressTDifference.goals.head.name, Some(Spec(Seq(differenceconsts))),
          Seq(SQLProgressTDifferenceIH1, SQLProgressTDifferenceIH2))))
    }

  }


  val rootinductionPS: g.ProofStep = g.applyTactic(progressObligation, rootInductionProgress)

  //TODO concrete tests that inspect the progress proof graph, once the file is executable

  //case split for union case (sometimes necessary, sometimes not)
  //(i.e. with a high timeout provers might be able to prove the case directly)

  val SQLProgressTUnion1 = goal(
    (('q1 === 'tvalue (~'t1)) &
      ('q2 === 'tvalue (~'t2)) &
      (~'q === 'Union ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Union-1")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val SQLProgressTUnion2 = goal(
    ((~'q1 === 'tvalue (~'t1)) &
      (forall(~'t2) | ('q2 ~= 'tvalue (~'t2))) &
      (~'q === 'Union ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Union-2")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val SQLProgressTUnion3 = goal(
    ((forall(~'t1) | ('q1 ~= 'tvalue (~'t1))) &
      (~'q === 'Union ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Union-3")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))


  // hard coded tactic for case distinction of union induction case
  //TODO refine empty list in argument to MockCaseDistinction
  val unionCaseDistinction = new MockCaseDistinction(Seq()) {
    override def apply[Obligation](obl: GenObligation[Spec, VeritasConstruct], produce: ObligationProducer[Spec, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val unioncase1: Obligation = produce.newObligation(fullSQLspec, SQLProgressTUnion1)

      val unioncase2: Obligation = produce.newObligation(fullSQLspec, SQLProgressTUnion2)

      val unioncase3: Obligation = produce.newObligation(fullSQLspec, SQLProgressTUnion3)

      Seq((unioncase1, CaseDistinctionCase[Spec, VeritasConstruct]("Union1", Some(Spec(Seq(unionconsts))), Seq())),
        (unioncase2, CaseDistinctionCase[Spec, VeritasConstruct]("Union2", Some(Spec(Seq(unionconsts))),
          Seq(SQLProgressTUnionIH2))),
        (unioncase3, CaseDistinctionCase[Spec, VeritasConstruct]("Union3", Some(Spec(Seq(unionconsts))),
          Seq(SQLProgressTUnionIH1))))
    }

  }

  val unioncaseobl = MockInduction.selectCase(SQLProgressTUnion.goals.head.name, g.requiredObls(rootinductionPS))
  g.applyTactic(unioncaseobl, unionCaseDistinction)


  //intersection and difference cases are completely analogous to union case
  //case split for intersection case (sometimes necessary, sometimes not)
  //(i.e. with a high timeout provers might be able to prove the case directly)

  val SQLProgressTIntersection1 = goal(
    (('q1 === 'tvalue (~'t1)) &
      ('q2 === 'tvalue (~'t2)) &
      (~'q === 'Intersection ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Intersection-1")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val SQLProgressTIntersection2 = goal(
    ((~'q1 === 'tvalue (~'t1)) &
      (forall(~'t2) | ('q2 ~= 'tvalue (~'t2))) &
      (~'q === 'Intersection ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Intersection-2")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val SQLProgressTIntersection3 = goal(
    ((forall(~'t1) | ('q1 ~= 'tvalue (~'t1))) &
      (~'q === 'Intersection ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Intersection-3")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  // hard coded tactic for case distinction of intersection induction case
  //TODO refine empty list in argument to MockCaseDistinction
  val intersectionCaseDistinction = new MockCaseDistinction(Seq()) {
    override def apply[Obligation](obl: GenObligation[Spec, VeritasConstruct], produce: ObligationProducer[Spec, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val intersectioncase1: Obligation = produce.newObligation(fullSQLspec, SQLProgressTIntersection1)

      val intersectioncase2: Obligation = produce.newObligation(fullSQLspec, SQLProgressTIntersection2)

      val intersectioncase3: Obligation = produce.newObligation(fullSQLspec, SQLProgressTIntersection3)

      Seq((intersectioncase1, CaseDistinctionCase[Spec, VeritasConstruct]("Intersection1", Some(Spec(Seq(intersectionconsts))), Seq())),
        (intersectioncase2, CaseDistinctionCase[Spec, VeritasConstruct]("Intersection2", Some(Spec(Seq(intersectionconsts))),
          Seq(SQLProgressTIntersectionIH2))),
        (intersectioncase3, CaseDistinctionCase[Spec, VeritasConstruct]("Intersection3", Some(Spec(Seq(intersectionconsts))),
          Seq(SQLProgressTIntersectionIH1))))
    }

  }

  val intersectioncaseobl = MockInduction.selectCase(SQLProgressTIntersection.goals.head.name, g.requiredObls(rootinductionPS))
  g.applyTactic(intersectioncaseobl, intersectionCaseDistinction)


  //case split for difference case (sometimes necessary, sometimes not)
  //(i.e. with a high timeout provers might be able to prove the case directly)

  val SQLProgressTDifference1 = goal(
    (('q1 === 'tvalue (~'t1)) &
      ('q2 === 'tvalue (~'t2)) &
      (~'q === 'Difference ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Difference-1")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val SQLProgressTDifference2 = goal(
    ((~'q1 === 'tvalue (~'t1)) &
      (forall(~'t2) | ('q2 ~= 'tvalue (~'t2))) &
      (~'q === 'Difference ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Difference-2")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  val SQLProgressTDifference3 = goal(
    ((forall(~'t1) | ('q1 ~= 'tvalue (~'t1))) &
      (~'q === 'Difference ('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-Difference-3")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo))))

  // hard coded tactic for case distinction of difference induction case
  //TODO refine empty list in argument to MockCaseDistinction
  val differenceCaseDistinction = new MockCaseDistinction(Seq()) {
    override def apply[Obligation](obl: GenObligation[Spec, VeritasConstruct], produce: ObligationProducer[Spec, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val differencecase1: Obligation = produce.newObligation(fullSQLspec, SQLProgressTDifference1)

      val differencecase2: Obligation = produce.newObligation(fullSQLspec, SQLProgressTDifference2)

      val differencecase3: Obligation = produce.newObligation(fullSQLspec, SQLProgressTDifference3)

      Seq((differencecase1, CaseDistinctionCase[Spec, VeritasConstruct]("Difference1", Some(Spec(Seq(differenceconsts))), Seq())),
        (differencecase2, CaseDistinctionCase[Spec, VeritasConstruct]("Difference2", Some(Spec(Seq(differenceconsts))),
          Seq(SQLProgressTDifferenceIH2))),
        (differencecase3, CaseDistinctionCase[Spec, VeritasConstruct]("Difference3", Some(Spec(Seq(differenceconsts))),
          Seq(SQLProgressTDifferenceIH1))))
    }

  }

  val differencecaseobl = MockInduction.selectCase(SQLProgressTDifference.goals.head.name, g.requiredObls(rootinductionPS))
  g.applyTactic(differencecaseobl, differenceCaseDistinction)



  // here, the SQL lemmas necessary for progress (selectFromWhere case) start

  val successfulLookup: Lemmas = lemma(
    ('StoreContextConsistent (~'TS, ~'TTC) &
      ('lookupContext (~'tn, ~'TTC) === 'someTType (~'tt))
      ).===>("successful-lookup")(
      exists(~'t) |
        ('lookupStore (~'tn, ~'TS) === 'someTable (~'t)))
  )

  //induction cases for successfulLookup
  val successfulLookupEmpty: Goals = goal(
    ((~'TS === 'emptyStore) &
      ('StoreContextConsistent (~'TS, ~'TTC)) &
      ('lookupContext (~'tn, ~'TTC) === 'someTType (~'tt))
      ).===>("successful-lookup-empty")(
      exists(~'t) |
        ('lookupStore (~'tn, ~'TS) === 'someTable (~'t))
    ))

  val successfulLookupBindConsts = consts('TSR ::> 'TStore)

  val successfulLookupBindIH: Axioms = axiom(
    ((~'TS === 'TSR) &
      ('StoreContextConsistent (~'TS, ~'TTC)) &
      ('lookupContext (~'tn, ~'TTC) === 'someTType (~'tt))
      ).===>("successful-lookup-bind-IH")(
      exists(~'t) |
        'lookupStore (~'tn, ~'TS) === 'someTable (~'t))
  )

  val successfulLookupBind: Goals = goal(
    ((~'TS === 'bindStore (~'tm, ~'t, 'TSR)) &
      ('StoreContextConsistent (~'TS, ~'TTC)) &
      ('lookupContext (~'tn, ~'TTC) === 'someTType (~'tt))
      ).===>("successful-lookup-bind")(
      exists(~'t) |
        'lookupStore (~'tn, ~'TS) === 'someTable (~'t))
  )

  val localBlocksuccessfulLookupBind = local(successfulLookupBindConsts, successfulLookupBindIH, successfulLookupBind)
  //end of induction cases for successfulLookup


  val welltypedLookup: Lemmas = lemma(
    ('StoreContextConsistent (~'TS, ~'TTC) &
      ('lookupStore (~'tn, ~'TS) === 'someTable (~'t)) &
      ('lookupContext (~'tn, ~'TTC) === 'someTType (~'tt))
      ).===>("welltyped-lookup")(
      'welltypedtable (~'tt, ~'t))
  )

  //induction cases for welltypedLookup
  val welltypedLookupEmpty: Goals = goal(
    ((~'TS === 'emptyStore) &
      ('StoreContextConsistent (~'TS, ~'TTC)) &
      ('lookupStore (~'tn, ~'TS) === 'someTable (~'t)) &
      ('lookupContext (~'tn, ~'TTC) === 'someTType (~'tt))
      ).===>("welltyped-lookup-empty")(
      'welltypedtable (~'tt, ~'t))
  )

  //TODO: later omit some of the reduncancy in the specification (i.e. not have two values with the same constructs)
  //for now, just duplicated some (parts of) axioms/lemmas/constdefs to make everything explicit
  val welltypedLookupConsts = consts('TSR ::> 'TStore)

  val welltypedLookupBindIH: Axioms = axiom(
    ((~'TS === 'TSR) &
      ('StoreContextConsistent (~'TS, ~'TTC)) &
      ('lookupStore (~'tn, ~'TS) === 'someTable (~'t)) &
      ('lookupContext (~'tn, ~'TTC) === 'someTType (~'tt))
      ).===>("welltyped-lookup-bind-IH")(
      'welltypedtable (~'tt, ~'t))
  )

  val welltypedLookupBind: Goals = goal(
    ((~'TS === 'bindStore (~'tm, ~'t, 'TSR)) &
      'StoreContextConsistent (~'TS, ~'TTC) &
      ('lookupStore (~'tn, ~'TS) === 'someTable (~'t)) &
      ('lookupContext (~'tn, ~'TTC) === 'someTType (~'tt))
      ).===>("welltyped-lookup")(
      'welltypedtable (~'tt, ~'t))
  )

  val localBlockwelltypedLookupBind = local(welltypedLookupConsts, welltypedLookupBindIH, welltypedLookupBind)
  //end of induction cases for welltypedLookup


  val filterPreservesType: Lemmas = lemma(
    ('welltypedtable (~'tt, ~'t)
      ).===>("filter-preserves-type")(
      'welltypedtable (~'tt, 'filterTable (~'t, ~'p)))
  ) //proved directly via filterRowsPreservesTable

  val filterRowsPreservesTable: Lemmas = lemma(
    ('welltypedRawtable (~'tt, ~'rt)
      ).===>("filterRows-preserves-table")(
      'welltypedRawtable (~'tt, 'filterRows (~'rt, ~'al, ~'p))
    ))

  // induction cases filterRowsPreservesTable
  val filterRowsPreservesTableTempty: Goals = goal(
    ((~'rt === 'tempty) &
      'welltypedRawtable (~'tt, ~'rt)
      ).===>("filterRows-preserves-table-tempty")(
      'welltypedRawtable (~'tt, 'filterRows (~'rt, ~'al, ~'p))
    ))

  val filterRowsPreservesTableTconsConsts = consts('rtr ::> 'RawTable)

  val filterRowsPreservesTableTconsIH: Axioms = axiom(
    ((~'rt === 'rtr) &
      'welltypedRawtable (~'tt, ~'rt)
      ).===>("filterRows-preserves-table-tcons-IH")(
      'welltypedRawtable (~'tt, 'filterRows (~'rt, ~'al, ~'p))
    ))

  val filterRowsPreservesTableTcons: Goals = goal(
    ((~'rt === 'tcons (~'r, 'rtr)) &
      'welltypedRawtable (~'tt, ~'rt)
      ).===>("filterRows-preserves-table-tcons")(
      'welltypedRawtable (~'tt, 'filterRows (~'rt, ~'al, ~'p))
    ))

  val localblockfilterRowsPreservesTableTcons = local(filterRowsPreservesTableTconsConsts, filterRowsPreservesTableTconsIH, filterRowsPreservesTableTcons)
  // end of induction cases filterRowsPreservesTable

  val projectTableProgress: Lemmas = lemma(
    ('welltypedtable (~'tt, ~'t) &
      ('projectType (~'s, ~'tt) === 'someTType (~'tt2))
      ).===>("projectTable-progress")(
      exists(~'t2) |
        'projectTable (~'s, ~'t) === 'someTable (~'t2))
  ) //proof by case distinction on 's (maybe not necessary?); list case by projectColsProgress

  val projectColsProgress: Lemmas = lemma(
    (('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('projectType ('list (~'al2), ~'tt) === 'someTType (~'tt2))
      ).===>("projectCols-progress")(
      exists(~'rt2) |
        'projectCols (~'al2, ~'al, ~'rt) === 'someRawTable (~'rt2))
  )

  //induction cases of projectColsProgress
  val projectColsProgressAempty: Goals = goal(
    ((~'al2 === 'aempty) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('projectType ('list (~'al2), ~'tt) === 'someTType (~'tt2))
      ).===>("projectCols-progress-aempty")(
      exists(~'rt2) |
        'projectCols (~'al2, ~'al, ~'rt) === 'someRawTable (~'rt2))
  )

  val projectColsProgressAconsConsts = consts('alr ::> 'AttrL)

  val projectColsProgressAconsIH: Axioms = axiom(
    ((~'al2 === 'alr) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('projectType ('list (~'al2), ~'tt) === 'someTType (~'tt2))
      ).===>("projectCols-progress-acons")(
      exists(~'rt2) |
        'projectCols (~'al2, ~'al, ~'rt) === 'someRawTable (~'rt2))
  )

  val projectColsProgressAcons: Goals = goal(
    ((~'al2 === 'acons (~'a, 'alr)) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('projectType ('list (~'al2), ~'tt) === 'someTType (~'tt2))
      ).===>("projectCols-progress-acons")(
      exists(~'rt2) |
        'projectCols (~'al2, ~'al, ~'rt) === 'someRawTable (~'rt2))
  ) //induction case requires lemma projectTypeImpliesFindCol

  val localblockprojectColsProgressAcons = local(projectColsProgressAconsConsts, projectColsProgressAconsIH, projectColsProgressAcons)
  //end of induction cases of projectColsProgress

  val projectTypeImpliesFindCol: Lemmas = lemma(
    (('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('projectTypeAttrL (~'al2, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al2))
      ).===>("projectType-implies-findCol")(
      exists(~'rt2) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt2))
  )

  //induction cases for projectTypeImpliesFindCol
  val projectTypeImpliesFindColAempty: Goals = goal(
    ((~'al2 === 'aempty) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('projectTypeAttrL (~'al2, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al2))
      ).===>("projectType-implies-findCol-aempty")(
      exists(~'rt2) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt2))
  )

  val projectTypeImpliesFindColAconsConsts = consts('alr ::> 'AttrL)

  val projectTypeImpliesFindColAconsIH: Axioms = axiom(
    ((~'al2 === 'alr) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('projectTypeAttrL (~'al2, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al2))
      ).===>("projectType-implies-findCol-acons-IH")(
      exists(~'rt2) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt2))
  )

  val projectTypeImpliesFindColAcons: Goals = goal(
    ((~'al2 === 'acons (~'a, 'alr)) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('projectTypeAttrL (~'al2, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al2))
      ).===>("projectType-implies-findCol-acons")(
      exists(~'rt2) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt2))
  ) //requires lemmas findColTypeImpliesfindCol and projectTypeAttrLImpliesfindAllColType

  val localblockprojectTypeImpliesFindColAcons = local(projectTypeImpliesFindColAconsConsts, projectTypeImpliesFindColAconsIH, projectTypeImpliesFindColAcons)
  //end of induction cases for projectTypeImpliesFindCol

  val findColTypeImpliesfindCol: Lemmas = lemma(
    (('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('findColType (~'n, ~'tt) === 'someFType (~'ft))
      ).===>("findColType-implies-findCol")(
      exists(~'rt) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt)
    ))

  // induction cases for findColTypeImpliesfindCol
  val findColTypeImpliesfindColAempty: Goals = goal(
    ((~'al === 'aempty) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('findColType (~'n, ~'tt) === 'someFType (~'ft))
      ).===>("findColType-implies-findCol-aempty")(
      exists(~'rt) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt)
    ))

  val findColTypeImpliesfindColAconsConsts = consts('alr ::> 'AttrL)

  val findColTypeImpliesfindColAconsIH: Axioms = axiom(
    ((~'al === 'alr) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('findColType (~'n, ~'tt) === 'someFType (~'ft))
      ).===>("findColType-implies-findCol-acons-IH")(
      exists(~'rt) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt)
    ))

  val findColTypeImpliesfindColACons: Goals = goal(
    ((~'al === 'acons (~'a, 'alr)) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('findColType (~'n, ~'tt) === 'someFType (~'ft))
      ).===>("findColType-implies-findCol-acons")(
      exists(~'rt) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt)
    )) //requires lemma dropFirstColRawPreservesWelltypedRaw

  val localblockfindColTypeImpliesfindColAcons = local(findColTypeImpliesfindColAconsConsts, findColTypeImpliesfindColAconsIH, findColTypeImpliesfindCol)
  // end of induction cases for findColTypeImpliesfindCol

  val dropFirstColRawPreservesWelltypedRaw: Lemmas = lemma(
    ((~'tt === 'ttcons (~'n, ~'ft, ~'ttr)) &
      ('welltypedRawtable (~'tt, ~'rt))
      ).===>("dropFirstColRaw-preserves-welltypedRaw")(
      'welltypedRawtable (~'ttr, 'dropFirstColRaw (~'rt))
    ))

  //induction cases for dropFirstColRawPreservesWelltypedRaw
  val dropFirstColRawPreservesWelltypedRawTempty: Goals = goal(
    ((~'rt === 'tempty) &
      (~'tt === 'ttcons (~'n, ~'ft, ~'ttr)) &
      ('welltypedRawtable (~'tt, ~'rt))
      ).===>("dropFirstColRaw-preserves-welltypedRaw-tempty")(
      'welltypedRawtable (~'ttr, 'dropFirstColRaw (~'rt))
    ))

  val dropFirstColRawPreservesWelltypedRawTconsConsts = consts('rts ::> 'RawTable)

  val dropFirstColRawPreservesWelltypedRawTconsIH: Axioms = axiom(
    ((~'rt === 'rtr) &
      (~'tt === 'ttcons (~'n, ~'ft, ~'ttr)) &
      ('welltypedRawtable (~'tt, ~'rt))
      ).===>("dropFirstColRaw-preserves-welltypedRaw-tcons-IH")(
      'welltypedRawtable (~'ttr, 'dropFirstColRaw (~'rt))
    ))

  val dropFirstColRawPreservesWelltypedRawTcons: Goals = goal(
    ((~'rt === 'tcons (~'r, 'rtr)) &
      (~'tt === 'ttcons (~'n, ~'ft, ~'ttr)) &
      ('welltypedRawtable (~'tt, ~'rt))
      ).===>("dropFirstColRaw-preserves-welltypedRaw-tcons")(
      'welltypedRawtable (~'ttr, 'dropFirstColRaw (~'rt))
    ))

  val localblockdropFirstColRawPreservesWelltypedRawTcons = local(dropFirstColRawPreservesWelltypedRawTconsConsts, dropFirstColRawPreservesWelltypedRawTconsIH, dropFirstColRawPreservesWelltypedRawTcons)
  //end of induction cases for dropFirstColRawPreservesWelltypedRaw


  val projectTypeAttrLImpliesfindAllColType: Lemmas = lemma(
    (('projectTypeAttrL (~'al, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al))
      ).===>("projectTypeAttrL-implies-findAllColType")(
      exists(~'ft) |
        'findColType (~'n, ~'tt) === 'someFType (~'ft)
    ))

  //induction cases projectTypeAttrLImpliesfindAllColType
  val projectTypeAttrLImpliesfindAllColTypeAempty: Goals = goal(
    ((~'al === 'aempty) &
      ('projectTypeAttrL (~'al, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al))
      ).===>("projectTypeAttrL-implies-findAllColType-aempty")(
      exists(~'ft) |
        'findColType (~'n, ~'tt) === 'someFType (~'ft)
    ))

  val projectTypeAttrLImpliesfindAllColTypeAconsConsts = consts('alr ::> 'AttrL)

  val projectTypeAttrLImpliesfindAllColTypeAconsIH: Axioms = axiom(
    ((~'al === 'alr) &
      ('projectTypeAttrL (~'al, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al))
      ).===>("projectTypeAttrL-implies-findAllColType-acons-IH")(
      exists(~'ft) |
        'findColType (~'n, ~'tt) === 'someFType (~'ft)
    ))

  val projectTypeAttrLImpliesfindAllColTypeAcons: Goals = goal(
    ((~'al === 'acons (~'a, 'aempty)) &
      ('projectTypeAttrL (~'al, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al))
      ).===>("projectTypeAttrL-implies-findAllColType-acons")(
      exists(~'ft) |
        'findColType (~'n, ~'tt) === 'someFType (~'ft)
    ))

  val localblockprojectTypeAttrLImpliesfindAllColTypeAcons = local(projectTypeAttrLImpliesfindAllColTypeAconsConsts, projectTypeAttrLImpliesfindAllColTypeAconsIH, projectTypeAttrLImpliesfindAllColTypeAcons)

  //end of induction cases projectTypeAttrLImpliesfindAllColType


}
