package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic._
import de.tu_darmstadt.veritas.VerificationInfrastructure.Evidence.AnyEvidenceChecker
import de.tu_darmstadt.veritas.VerificationInfrastructure.ProofGraph.ProofEdges
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}
import org.scalatest.FunSuite

/**
  * Created by sylvia on 28/02/2017.
  */
class SQLProgressProofGraph extends FunSuite {

  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._
  import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._
  import de.tu_darmstadt.veritas.inputdsl.ProofDSL._

  import de.tu_darmstadt.veritas.inputdsl.SQLDefs._

  // We instantiate S = Spec and P = VeritasConstruct
  // When we construct a Transformer that reuses our previous transformations to TPTP, we
  // might have to explicitly construct Module(s).


  // just an empty test implementation of current ProofGraph interface
  class TestProofGraph extends ProofGraph[Spec, VeritasConstruct] {
    override def addRootObligation(obl: Obligation[Spec, VeritasConstruct]): Unit = ???

    override def removeRootObligation(step: Obligation[Spec, VeritasConstruct]): Unit = ???

    override def applyTactic(obl: Obligation[Spec, VeritasConstruct], tactic: Tactic[Spec, VeritasConstruct]): ProofStep[Spec, VeritasConstruct] = ???

    override def unapplyTactic(obl: Obligation[Spec, VeritasConstruct]): Unit = ???

    override def setVerifiedBy(step: ProofStep[Spec, VeritasConstruct], result: GenStepResult[Spec, VeritasConstruct]): Unit = ???

    override def unsetVerifiedBy(step: ProofStep[Spec, VeritasConstruct]): Unit = ???

    override def rootObligations: Iterable[Obligation[Spec, VeritasConstruct]] = ???

    /** Yields proof step if any */
    override def appliedStep(obl: Obligation[Spec, VeritasConstruct]): Option[ProofStep[Spec, VeritasConstruct]] = ???

    /** Yields required subobligations */
    override def requiredObls(step: ProofStep[Spec, VeritasConstruct]): Iterable[(Obligation[Spec, VeritasConstruct], EdgeLabel)] = ???

    /** Yields proof steps that require the given obligation */
    override def requiringSteps(obligation: Obligation[Spec, VeritasConstruct]): Iterable[(ProofStep[Spec, VeritasConstruct], EdgeLabel)] = ???

    /** Yields the obligation the proof step was applied to */
    override def targetedObl(step: ProofStep[Spec, VeritasConstruct]): Obligation[Spec, VeritasConstruct] = ???

    override def verifiedBy(step: ProofStep[Spec, VeritasConstruct]): Option[GenStepResult[Spec, VeritasConstruct]] = ???

    override var defaultEvidencenChecker: AnyEvidenceChecker = _
  }


  case class Spec(content: Seq[VeritasConstruct]) extends Ordered[Spec] {
    val ord = Ordering.Iterable[VeritasConstruct](Ordering.ordered[VeritasConstruct](x => x))
    override def compare(that: Spec): Int = ord.compare(this.content, that.content)
  }

  val VeriSolve = Solve[Spec, VeritasConstruct]()
  type VeriPS = Obligation[Spec, VeritasConstruct]
  type VeriTactic = Tactic[Spec, VeritasConstruct]
  type VeriStructuralInduction = StructuralInduction[Spec, VeritasConstruct]

  def makeVeriStructInduct(spec: Spec): VeriStructuralInduction =
    StructuralInduction[Spec, VeritasConstruct](spec)

  def makeVeriPS(spec: Spec, goal: VeritasConstruct): VeriPS =
    Obligation[Spec, VeritasConstruct](spec, goal)

  val fulltestspec: Spec = Spec(Tables.defs ++ TableAux.defs ++ TStore.defs ++ TContext.defs ++
    Syntax.defs ++ Semantics.defs ++ TypeSystem.defs ++ TypeSystemInv.defs ++ SoundnessAuxDefs.defs)

  val progressroot = makeVeriPS(fulltestspec, SQLProgress)

  //Mock tactic for structural induction from which we can
  // inherit mock induction tactics where generated goals are hard-coded (overriding the apply method)
  class MockStructuralInduction(inductionvar: Spec) extends Tactic[Spec, VeritasConstruct] {
    //TODO we might have to refine the verifier call for induction once we really support this via a prover
    override def verifyStep(step: VeriPS, edges: ProofEdges[Spec, VeritasConstruct], verifier: Verifier[Spec, VeritasConstruct]): GenStepResult[Spec, VeritasConstruct] =
      super.verifyStep(step, edges, verifier)


    override def compare(that: Tactic[Spec, VeritasConstruct]): Int = that match {
      case that: StructuralInduction[Spec, VeritasConstruct] => this.inductionvar compare that.inductionvar
      case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
    }

    def apply(obl: Obligation[Spec, VeritasConstruct]): Iterable[(Obligation[Spec, VeritasConstruct], EdgeLabel)] = Seq()
  }

  class MockCaseDistinction() extends Tactic[Spec, VeritasConstruct] {

    override def compare(that: Tactic[Spec, VeritasConstruct]): Int = that match {
      case that: CaseDistinction[Spec, VeritasConstruct] => 0
      case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
    }

    def apply(obl: Obligation[Spec, VeritasConstruct]): Iterable[(Obligation[Spec, VeritasConstruct], EdgeLabel)] = Seq()
  }

  // Start a proof graph with a root goal (SQL progress)
  val SQLProgressProofGraph = new TestProofGraph()

  SQLProgressProofGraph.addRootObligation(progressroot)

  //Add structural induction on q (query) to the graph by applying an
  // induction tactic (here: mock tactic that simply generates
  // exactly the induction cases we require
  SQLProgressProofGraph.applyTactic(progressroot, RootInduction(Spec(Seq(MetaVar("q")))))

  //concrete induction for the root goal (progress) where the generated goals are just hard-coded
  case class RootInduction(inductionvar: Spec) extends MockStructuralInduction(inductionvar) {
    val tvaluecase = makeVeriPS(fulltestspec, SQLProgressTtvalue)

    val selectfromwherecase = makeVeriPS(fulltestspec, SQLProgressTselectFromWhere)

    val unioncase = makeVeriPS(fulltestspec, localblockunion)

    val intersectioncase = makeVeriPS(fulltestspec, localblockintersection)

    val differencecase = makeVeriPS(fulltestspec, localblockdifference)

    override def apply(obl: Obligation[Spec, VeritasConstruct]): Iterable[(Obligation[Spec, VeritasConstruct], EdgeLabel)] =
      Seq((tvaluecase, StructInductCase[VeritasConstruct](SQLProgressTtvalue.goals.head.name, Seq())),
        (selectfromwherecase, StructInductCase[VeritasConstruct](SQLProgressTselectFromWhere.goals.head.name, Seq())),
        (unioncase, StructInductCase[VeritasConstruct](SQLProgressTUnion.goals.head.name,
          Seq(SQLProgressTUnionIH1, SQLProgressTUnionIH2))),
        (intersectioncase, StructInductCase[VeritasConstruct](SQLProgressTIntersection.goals.head.name,
          Seq(SQLProgressTIntersectionIH1, SQLProgressTIntersectionIH2))),
        (differencecase, StructInductCase[VeritasConstruct](SQLProgressTDifference.goals.head.name,
          Seq(SQLProgressTDifferenceIH1, SQLProgressTDifferenceIH2))))
  }


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

  val localblockunioncase1 = local(unionconsts, SQLProgressTUnion1)
  //TODO: How to deal with the local blocks here?
  val unioncase1node = makeVeriPS(fulltestspec, localblockunioncase1)
  //TODO: add edges

  val localblockunioncase2 = local(unionconsts, SQLProgressTUnionIH2, SQLProgressTUnion2)
  val unioncase2node = makeVeriPS(fulltestspec, localblockunioncase2)

  val localblockunioncase3 = local(unionconsts, SQLProgressTUnionIH1, SQLProgressTUnion3)
  val unioncase3node = makeVeriPS(fulltestspec, localblockunioncase3)


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

  val localblockintersectioncase1 = local(intersectionconsts, SQLProgressTIntersection1)
  val intersectioncase1node = makeVeriPS(fulltestspec, localblockintersectioncase1)

  val localblockintersectioncase2 = local(intersectionconsts, SQLProgressTIntersectionIH2, SQLProgressTIntersection2)
  val intersectioncase2node = makeVeriPS(fulltestspec, localblockintersectioncase2)

  val localblockintersectioncase3 = local(intersectionconsts, SQLProgressTIntersectionIH1, SQLProgressTIntersection3)
  val intersectioncase3node = makeVeriPS(fulltestspec, localblockintersectioncase3)


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

  val localblockdifferencecase1 = local(differenceconsts, SQLProgressTDifference1)
  val differencecase1node = makeVeriPS(fulltestspec, localblockdifferencecase1)

  val localblockdifferencecase2 = local(differenceconsts, SQLProgressTDifferenceIH2, SQLProgressTDifference2)
  val differencecase2node = makeVeriPS(fulltestspec, localblockdifferencecase2)

  val localblockdifferencecase3 = local(differenceconsts, SQLProgressTDifferenceIH1, SQLProgressTDifference3)
  val differencecase3node = makeVeriPS(fulltestspec, localblockdifferencecase3)


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
    ( (~'rt === 'tempty) &
      'welltypedRawtable (~'tt, ~'rt)
      ).===>("filterRows-preserves-table-tempty")(
      'welltypedRawtable (~'tt, 'filterRows (~'rt, ~'al, ~'p))
    ))

  val filterRowsPreservesTableTconsConsts = consts('rtr ::> 'RawTable)

  val filterRowsPreservesTableTconsIH: Axioms = axiom(
    ( (~'rt === 'rtr) &
      'welltypedRawtable (~'tt, ~'rt)
      ).===>("filterRows-preserves-table-tcons-IH")(
      'welltypedRawtable (~'tt, 'filterRows (~'rt, ~'al, ~'p))
    ))

  val filterRowsPreservesTableTcons: Goals = goal(
    ( (~'rt === 'tcons(~'r, 'rtr)) &
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
    ( (~'al2 === 'aempty) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('projectType ('list (~'al2), ~'tt) === 'someTType (~'tt2))
      ).===>("projectCols-progress-aempty")(
      exists(~'rt2) |
        'projectCols (~'al2, ~'al, ~'rt) === 'someRawTable (~'rt2))
  )

  val projectColsProgressAconsConsts = consts('alr ::> 'AttrL)

  val projectColsProgressAconsIH: Axioms = axiom(
    ( (~'al2 === 'alr) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('projectType ('list (~'al2), ~'tt) === 'someTType (~'tt2))
      ).===>("projectCols-progress-acons")(
      exists(~'rt2) |
        'projectCols (~'al2, ~'al, ~'rt) === 'someRawTable (~'rt2))
  )

  val projectColsProgressAcons: Goals = goal(
    ( (~'al2 === 'acons(~'a, 'alr)) &
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
    ( (~'al2 === 'aempty) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('projectTypeAttrL (~'al2, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al2))
      ).===>("projectType-implies-findCol-aempty")(
      exists(~'rt2) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt2))
  )

  val projectTypeImpliesFindColAconsConsts = consts('alr ::> 'AttrL)

  val projectTypeImpliesFindColAconsIH: Axioms = axiom(
    ( (~'al2 === 'alr) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('projectTypeAttrL (~'al2, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al2))
      ).===>("projectType-implies-findCol-acons-IH")(
      exists(~'rt2) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt2))
  )

  val projectTypeImpliesFindColAcons: Goals = goal(
    ( (~'al2 === 'acons(~'a, 'alr)) &
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
    ( (~'al === 'aempty) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('findColType (~'n, ~'tt) === 'someFType (~'ft))
      ).===>("findColType-implies-findCol-aempty")(
      exists(~'rt) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt)
    ))

  val findColTypeImpliesfindColAconsConsts = consts('alr ::> 'AttrL)

  val findColTypeImpliesfindColAconsIH: Axioms = axiom(
    ( (~'al === 'alr) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('findColType (~'n, ~'tt) === 'someFType (~'ft))
      ).===>("findColType-implies-findCol-acons-IH")(
      exists(~'rt) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt)
    ))

  val findColTypeImpliesfindColACons: Goals = goal(
    ( (~'al === 'acons(~'a, 'alr)) &
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
    ( (~'rt === 'tempty) &
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
    ( (~'rt === 'tcons(~'r, 'rtr)) &
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
    ( (~'al === 'aempty) &
      ('projectTypeAttrL (~'al, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al))
      ).===>("projectTypeAttrL-implies-findAllColType-aempty")(
      exists(~'ft) |
        'findColType (~'n, ~'tt) === 'someFType (~'ft)
    ))

  val projectTypeAttrLImpliesfindAllColTypeAconsConsts = consts('alr ::> 'AttrL)

  val projectTypeAttrLImpliesfindAllColTypeAconsIH: Axioms = axiom(
    ( (~'al === 'alr) &
      ('projectTypeAttrL (~'al, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al))
      ).===>("projectTypeAttrL-implies-findAllColType-acons-IH")(
      exists(~'ft) |
        'findColType (~'n, ~'tt) === 'someFType (~'ft)
    ))

  val projectTypeAttrLImpliesfindAllColTypeAcons: Goals = goal(
    ( (~'al === 'acons(~'a, 'aempty)) &
      ('projectTypeAttrL (~'al, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al))
      ).===>("projectTypeAttrL-implies-findAllColType-acons")(
      exists(~'ft) |
        'findColType (~'n, ~'tt) === 'someFType (~'ft)
    ))

  val localblockprojectTypeAttrLImpliesfindAllColTypeAcons = local(projectTypeAttrLImpliesfindAllColTypeAconsConsts, projectTypeAttrLImpliesfindAllColTypeAconsIH, projectTypeAttrLImpliesfindAllColTypeAcons)

  //end of induction cases projectTypeAttrLImpliesfindAllColType


}
