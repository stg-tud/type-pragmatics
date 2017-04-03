package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic._
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{Failure, Finished, TPTPVampireVerifier, TSTPProof}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}
import org.scalatest.FunSuite

//This object contains all MockTactics and hand-coded obligations for the SQL soundness proof graph
object SQLSoundnessProofGraph {

  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._
  import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._
  import de.tu_darmstadt.veritas.inputdsl.ProofDSL._

  import de.tu_darmstadt.veritas.inputdsl.SQLDefs._


  val fullSQLspec: Module = Module("SQLspec", Seq(), Tables.defs ++ TableAux.defs ++ TStore.defs ++ TContext.defs ++
    Syntax.defs ++ Semantics.defs ++ TypeSystem.defs ++ SoundnessAuxDefs.defs)

  //Mock tactics
  // class for creating mock induction tactics, with convenience methods like selectCase
  class MockInduction(inductionvar: VeritasConstruct) extends Tactic[VeritasConstruct, VeritasConstruct] {

    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasConstruct],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] =
      Seq()

    override def compare(that: Tactic[VeritasConstruct, VeritasConstruct]): Int = ???

  }

  object MockInduction {
    def selectCase[Obligation](name: String, required: Iterable[(Obligation, EdgeLabel)]): Obligation =
      required.find(_._2.asInstanceOf[StructInductCase[VeritasConstruct]].casename == name).get._1
  }


  //class for creating mock case distinctions
  class MockCaseDistinction(cases: Seq[VeritasConstruct]) extends Tactic[VeritasConstruct, VeritasConstruct] {

    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasConstruct],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] =
      Seq()

    override def compare(that: Tactic[VeritasConstruct, VeritasConstruct]): Int = ???
  }

  object MockCaseDistinction {
    def selectCase[Obligation](name: String, required: Iterable[(Obligation, EdgeLabel)]): Obligation =
      required.find(_._2.asInstanceOf[CaseDistinctionCase[VeritasConstruct]].casename == name).get._1
  }


  // Apply structural induction to progress root via ad-hoc instance of MockInduction,
  // where the goals that are supposed to be generated are just hard-coded

  object rootInductionProgress extends MockInduction(MetaVar("q")) {

    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasConstruct],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val tvaluecase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTtvalue)

      val selectfromwherecase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTselectFromWhere)

      val unioncase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTUnion)

      val intersectioncase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTIntersection)

      val differencecase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTDifference)


      val SQLProgressTUnionIH1 =
        ((!'isValue ('q1)) &
          ('TTC |- 'q1 :: 'TT) &
          'StoreContextConsistent ('TS, 'TTC)
          ).===>("SQL-Progress-T-Union-IH1")(
          exists(~'qo) |
            ('reduce ('q1, 'TS) === 'someQuery (~'qo))
        )

      val SQLProgressTUnionIH2 =
        ((!'isValue ('q2)) &
          ('TTC |- 'q2 :: 'TT) &
          'StoreContextConsistent ('TS, 'TTC)
          ).===>("SQL-Progress-T-Union-IH2")(
          exists(~'qo) |
            ('reduce ('q2, 'TS) === 'someQuery (~'qo)))

      val SQLProgressTIntersectionIH1 =
        ((!'isValue ('q1)) &
          ('TTC |- 'q1 :: 'TT) &
          'StoreContextConsistent ('TS, 'TTC)
          ).===>("SQL-Progress-T-Intersection-IH1")(
          exists(~'qo) |
            ('reduce ('q1, 'TS) === 'someQuery (~'qo)))


      val SQLProgressTIntersectionIH2 =
        ((!'isValue ('q2)) &
          ('TTC |- 'q2 :: 'TT) &
          'StoreContextConsistent ('TS, 'TTC)
          ).===>("SQL-Progress-T-Intersection-IH2")(
          exists(~'qo) |
            ('reduce ('q2, 'TS) === 'someQuery (~'qo)))

      val SQLProgressTDifferenceIH1 =
        ((!'isValue ('q1)) &
          ('TTC |- 'q1 :: 'TT) &
          'StoreContextConsistent ('TS, 'TTC)
          ).===>("SQL-Progress-T-Difference-IH1")(
          exists(~'qo) |
            ('reduce ('q1, 'TS) === 'someQuery (~'qo)))

      val SQLProgressTDifferenceIH2 =
        ((!'isValue ('q2)) &
          ('TTC |- 'q2 :: 'TT) &
          'StoreContextConsistent ('TS, 'TTC)
          ).===>("SQL-Progress-T-Difference-IH2")(
          exists(~'qo) |
            ('reduce ('q2, 'TS) === 'someQuery (~'qo))
        )

      Seq((tvaluecase, StructInductCase[VeritasConstruct](SQLProgressTtvalue.goals.head.name,
        None,
        InductionHypotheses[VeritasConstruct](Axioms(Seq())))),
        (selectfromwherecase, StructInductCase[VeritasConstruct](SQLProgressTselectFromWhere.goals.head.name,
          None,
          InductionHypotheses[VeritasConstruct](Axioms(Seq())))),
        (unioncase, StructInductCase[VeritasConstruct](SQLProgressTUnion.goals.head.name,
          Some(FixedVars(unionconsts)),
          InductionHypotheses[VeritasConstruct](Axioms(Seq(SQLProgressTUnionIH1, SQLProgressTUnionIH2))))),
        (intersectioncase, StructInductCase[VeritasConstruct](SQLProgressTIntersection.goals.head.name,
          Some(FixedVars(intersectionconsts)),
          InductionHypotheses[VeritasConstruct](Axioms(Seq(SQLProgressTIntersectionIH1, SQLProgressTIntersectionIH2))))),
        (differencecase, StructInductCase[VeritasConstruct](SQLProgressTDifference.goals.head.name,
          Some(FixedVars(differenceconsts)),
          InductionHypotheses[VeritasConstruct](Axioms(Seq(SQLProgressTDifferenceIH1, SQLProgressTDifferenceIH2))))))
    }
  }

  //abstract case splits for union/intersection/difference case (sometimes necessary, sometimes not)
  //(i.e. with a high timeout provers might be able to prove the case directly)

  val unionsym = 'Union
  val sunion = "Union"
  val intersectionsym = 'Intersection
  val sintersection = "Intersection"
  val differencesym = 'Difference
  val sdifference = "Difference"

  val case1pred: Seq[TypingRuleJudgment] = ('q1 === 'tvalue (~'t1)) & ('q2 === 'tvalue (~'t2))
  val case2pred: Seq[TypingRuleJudgment] = (~'q1 === 'tvalue (~'t1)) & (forall(~'t2) | ('q2 ~= 'tvalue (~'t2)))
  val case3pred: Seq[TypingRuleJudgment] = Seq(forall(~'t1) | ('q1 ~= 'tvalue (~'t1)))

  def setconsts = consts('q1 ::> 'Query,
    'q2 ::> 'Query,
    'TS ::> 'TStore,
    'TTC ::> 'TTContext,
    'TT ::> 'TType)


  def mkSQLProgressTSetCaseIH(i: Int, setname: String, indvar: Symbol) =
    (!'isValue (indvar) &
      ('TTC |- indvar :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>("SQL-Progress-T-" + setname + "-IH" + i)(
      exists(~'qo) |
        ('reduce (indvar, 'TS) === 'someQuery (~'qo)))


  def mkSQLProgressTSetCase(i: Int, setsym: Symbol, setname: String, casepreds: Seq[TypingRuleJudgment]) =
    goal((casepreds &
      (~'q === setsym('q1, 'q2)) &
      (!'isValue (~'q)) &
      ('TTC |- ~'q :: 'TT) &
      'StoreContextConsistent ('TS, 'TTC)
      ).===>(s"SQL-Progress-T-$setname-${i + 1}")(
      exists(~'qo) |
        ('reduce (~'q, 'TS) === 'someQuery (~'qo)))
    )

  // hard coded tactic for case distinction of union/intersection/difference induction case
  case class SetCaseDistinction(setsym: Symbol, setname: String)
    extends MockCaseDistinction(Seq(case1pred, case2pred, case3pred) map
      ((stj: Seq[TypingRuleJudgment]) => Goals(Seq(TypingRule("casepreds", Seq(), stj)), None))) {

    val casepreds = Seq(case1pred, case2pred, case3pred)

    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasConstruct],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasConstruct, Obligation]):
    Iterable[(Obligation, EdgeLabel)] = {
      def mkcase(i: Int): Obligation = produce.newObligation(fullSQLspec,
        mkSQLProgressTSetCase(i, setsym, setname, casepreds(i)))

      Seq((mkcase(0), CaseDistinctionCase[VeritasConstruct](setname + "1",
        Some(FixedVars(setconsts)),
        InductionHypotheses[VeritasConstruct](Axioms(Seq())))),
        (mkcase(1), CaseDistinctionCase[VeritasConstruct](setname + "2",
          Some(FixedVars(setconsts)),
          InductionHypotheses[VeritasConstruct](Axioms(Seq(mkSQLProgressTSetCaseIH(2, setname, 'q2)))))),
        (mkcase(2), CaseDistinctionCase[VeritasConstruct](setname + "3",
          Some(FixedVars(setconsts)),
          InductionHypotheses[VeritasConstruct](Axioms(Seq(mkSQLProgressTSetCaseIH(1, setname, 'q1)))))))
    }

  }

}


/**
  * Constructing the SQL soundness proof graph and tests on the graph
  */
class SQLSoundnessProofGraph extends FunSuite {

  import SQLSoundnessProofGraph._
  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._
  import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._
  import de.tu_darmstadt.veritas.inputdsl.ProofDSL._

  import de.tu_darmstadt.veritas.inputdsl.SQLDefs._


  // We instantiate S = VeritasConstruct (should be a Module) and P = VeritasConstruct (Should be Goal/local)

  val file = File.createTempFile("sql-progress-proof-store", "")
  file.delete()
  file.mkdir()
  println(s"Test entity store: $file")


  val g: ProofGraphXodus[VeritasConstruct, VeritasConstruct] =
    new ProofGraphXodus[VeritasConstruct, VeritasConstruct](file)

  //register all the necessary property types
  PropertyTypes.registerPropertyType[VeritasConstruct](g.store)
  PropertyTypes.registerPropertyType[Module](g.store)
  PropertyTypes.registerPropertyType[Goals](g.store)
  PropertyTypes.registerPropertyType[rootInductionProgress.type](g.store)
  PropertyTypes.registerPropertyType[StructInductCase[VeritasConstruct]](g.store)
  PropertyTypes.registerPropertyType[SetCaseDistinction](g.store)
  PropertyTypes.registerPropertyType[CaseDistinctionCase[VeritasConstruct]](g.store)
  PropertyTypes.registerPropertyType[Finished[_, _]](g.store)
  PropertyTypes.registerPropertyType[Failure[_, _]](g.store)
  PropertyTypes.registerPropertyType[TSTPProof](g.store)


  val testGoal: Goals = goal(===>("test")('p ('x) && 'q ('x) || 't ('x)))
  val testObligation: g.Obligation = g.newObligation(Module("empty", Seq(), Seq()), testGoal)

  test("Storing and finding the test obligation") {
    g.storeObligation("test", testObligation)

    val r = g.findObligation("test")

    assert(r.get.spec == testObligation.spec)
    assert(r.get.goal == testObligation.goal)
  }

  test("Unstoring the test obligation") {

    //first check whether obligation is still there
    val ro = g.findObligation("test")

    assert(ro.get.spec == testObligation.spec)
    assert(ro.get.goal == testObligation.goal)

    g.unstoreObligation(testObligation)
    val r = g.findObligation("test")

    assert(r == None)
  }


  val progressObligation: g.Obligation = g.newObligation(fullSQLspec, SQLProgress)
  g.storeObligation("SQL progress", progressObligation)


  test("Storing and finding the progress obligation") {
    val r = g.findObligation("SQL progress")

    assert(r.get.spec == progressObligation.spec)
    assert(r.get.goal == progressObligation.goal)
  }


  val rootinductionPS: g.ProofStep = g.applyTactic(progressObligation, rootInductionProgress)

  test("All root induction cases are retrievable") {
    val obls = g.requiredObls(rootinductionPS)
    val tvaluecase = MockInduction.selectCase(SQLProgressTtvalue.goals.head.name, obls)
    val selcase = MockInduction.selectCase(SQLProgressTselectFromWhere.goals.head.name, obls)
    val unioncase = MockInduction.selectCase(SQLProgressTUnion.goals.head.name, obls)
    val intersectioncase = MockInduction.selectCase(SQLProgressTIntersection.goals.head.name, obls)
    val differencecase = MockInduction.selectCase(SQLProgressTDifference.goals.head.name, obls)

    assert(tvaluecase.spec == fullSQLspec)
    assert(tvaluecase.goal == SQLProgressTtvalue)

    assert(selcase.spec == fullSQLspec)
    assert(selcase.goal == SQLProgressTselectFromWhere)

    assert(unioncase.spec == fullSQLspec)
    assert(unioncase.goal == SQLProgressTUnion)

    assert(intersectioncase.spec == fullSQLspec)
    assert(intersectioncase.goal == SQLProgressTIntersection)

    assert(differencecase.spec == fullSQLspec)
    assert(differencecase.goal == SQLProgressTDifference)
  }

  //apply simply Solve-tactic to t-value base case
  val tvaluecaseobl = MockInduction.selectCase(SQLProgressTtvalue.goals.head.name, g.requiredObls(rootinductionPS))
  val tvaluecasePS = g.applyTactic(tvaluecaseobl, Solve[VeritasConstruct, VeritasConstruct])

  test("T-value base case is provable (using Vampire 4.1, 5 sec)") {
    val simpleVerifier = new TPTPVampireVerifier(5)

    val result = g.verifyProofStep(tvaluecasePS, simpleVerifier)
    assert(result.status.isInstanceOf[Finished[_, _]])
    assert(result.status.isVerified)
    assert(result.errorMsg.isEmpty)
    assert(result.evidence.nonEmpty)

    //println(result.status.asInstanceOf[Finished[_,_]].status.proverResult.summaryDetails)
  }

  // Case distinctions for Union, Intersection, Difference cases
  val unionCaseDistinction = SetCaseDistinction(unionsym, sunion)

  val unioncaseobl = MockInduction.selectCase(SQLProgressTUnion.goals.head.name, g.requiredObls(rootinductionPS))
  val unioncasePS = g.applyTactic(unioncaseobl, unionCaseDistinction)

  val intersectionCaseDistinction = SetCaseDistinction(intersectionsym, sintersection)

  val intersectioncaseobl = MockInduction.selectCase(SQLProgressTIntersection.goals.head.name, g.requiredObls(rootinductionPS))
  val intersectioncasePS = g.applyTactic(intersectioncaseobl, intersectionCaseDistinction)

  val differenceCaseDistinction = SetCaseDistinction(differencesym, sdifference)

  val differencecaseobl = MockInduction.selectCase(SQLProgressTDifference.goals.head.name, g.requiredObls(rootinductionPS))
  val differencecasePS = g.applyTactic(differencecaseobl, differenceCaseDistinction)

  test("Applying case distinctions worked as desired") {
    val unionobls = g.requiredObls(unioncasePS)
    val union1 = MockCaseDistinction.selectCase("Union1", unionobls)
    val intersectionobls = g.requiredObls(intersectioncasePS)
    val intersection3 = MockCaseDistinction.selectCase("Intersection3", intersectionobls)
    val differenceobls = g.requiredObls(differencecasePS)
    val difference2 = MockCaseDistinction.selectCase("Difference2", differenceobls)

    assert(union1.goal == mkSQLProgressTSetCase(0, unionsym, sunion, case1pred))
    assert(intersection3.goal == mkSQLProgressTSetCase(2, intersectionsym, sintersection, case3pred))
    assert(difference2.goal == mkSQLProgressTSetCase(1, differencesym, sdifference, case2pred))

    //compare some induction hypotheses

    val obls = g.requiredObls(rootinductionPS)
    val unioncase = MockInduction.selectCase(SQLProgressTUnion.goals.head.name, obls)
    val intersectioncase = MockInduction.selectCase(SQLProgressTIntersection.goals.head.name, obls)
    val differencecase = MockInduction.selectCase(SQLProgressTDifference.goals.head.name, obls)

    val unionps = g.appliedStep(unioncase).get
    val unionedges = g.requiredObls(unionps).toSeq
    assert(unionedges.size == 3)
    assert(unionedges(1)._2.asInstanceOf[CaseDistinctionCase[VeritasConstruct]].ihs.ihs == SQLProgressTUnionIH2)

  }

  //apply Solve tactic to all of the cases
  val setobls = g.requiredObls(unioncasePS) ++
    g.requiredObls(intersectioncasePS) ++ g.requiredObls(differencecasePS)

  val setPS = for ((o, e) <- setobls) yield {
    g.applyTactic(o, Solve[VeritasConstruct, VeritasConstruct])
  }


  test("Timeout for individual set cases (SQL progress proof), using Vampire 4.1, 3 sec") {
    val simpleVerifier = new TPTPVampireVerifier(1)

    for (ps <- setPS) {
      val result = g.verifyProofStep(ps, simpleVerifier)

      assert(result.status.isInstanceOf[Finished[_, _]])
      assert(result.errorMsg.nonEmpty)
      assert(result.errorMsg.get == "Time limit")
    }
  }

  test("Proving a single set case") {
    val simpleVerifier = new TPTPVampireVerifier(30, "4.0")

    val result = g.verifyProofStep(setPS.head, simpleVerifier)

    println(result.status)
    assert(result.status.isInstanceOf[Finished[_, _]])
    assert(result.status.isVerified)
  }

  test("Proving all individual set cases with Vampire 4.0") {
    val simpleVerifier = new TPTPVampireVerifier(30, "4.0")

    for (ps <- setPS) {
      val result = g.verifyProofStep(ps, simpleVerifier)

      assert(result.status.isInstanceOf[Finished[_, _]])
      assert(result.status.isVerified)
      assert(result.errorMsg.isEmpty)
      assert(result.evidence.nonEmpty)
    }
  }


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
