package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic._
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{VerifierFailure, Finished, TPTPVampireVerifier, TSTPProof}
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

  //Mock tactxics
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

  //class for creating mock lemma generation tactics
  case class MockLemmaApplication(lemmas: Seq[Lemmas]) extends Tactic[VeritasConstruct, VeritasConstruct] {
    /**
      * applying a tactic to a ProofStep returns the edges generated from this application
      * edges include edge labels and sub-ProofSteps
      * caller has to decide whether the edges will be integrated into a proof graph or not
      *
      * @param obl
      * @param obllabels labels from edges that lead to the given obligation (for propagating proof info if necessary)
      * @throws TacticApplicationException
      * @return
      */
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasConstruct],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] =
      for (lem <- lemmas) yield
        produce.newObligation(fullSQLspec, Goals(lem.lemmas, lem.timeout)) -> LemmaApplication(lem.lemmas.head.name)

    override def compare(that: Tactic[VeritasConstruct, VeritasConstruct]): Int = ???
  }

  object MockLemmaApplication {
    def selectLemma[Obligation](name: String, required: Iterable[(Obligation, EdgeLabel)]): Obligation =
      required.find(_._2.asInstanceOf[LemmaApplication[VeritasConstruct]].lemmaname == name).get._1
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

  object successfulLookupInduction extends MockInduction(MetaVar("TS")) {

    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasConstruct],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val successfulLookupEmptyObl = produce.newObligation(fullSQLspec, successfulLookupEmpty)
      val successfulLookupBindObl = produce.newObligation(fullSQLspec, successfulLookupBind)

      Seq((successfulLookupEmptyObl, StructInductCase[VeritasConstruct](successfulLookupEmpty.goals.head.name,
        None,
        InductionHypotheses[VeritasConstruct](Axioms(Seq())))),
        (successfulLookupBindObl, StructInductCase[VeritasConstruct](successfulLookupBind.goals.head.name,
          Some(FixedVars(successfulLookupBindConsts)),
          InductionHypotheses[VeritasConstruct](successfulLookupBindIH))))

    }
  }


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


  object welltypedLookupInduction extends MockInduction(MetaVar("TS")) {

    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasConstruct],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val welltypedLookupEmptyObl = produce.newObligation(fullSQLspec, welltypedLookupEmpty)
      val welltypedLookupBindObl = produce.newObligation(fullSQLspec, welltypedLookupBind)

      Seq((welltypedLookupEmptyObl, StructInductCase[VeritasConstruct](welltypedLookupEmpty.goals.head.name,
        None,
        InductionHypotheses[VeritasConstruct](Axioms(Seq())))),
        (welltypedLookupBindObl, StructInductCase[VeritasConstruct](welltypedLookupBind.goals.head.name,
          Some(FixedVars(welltypedLookupConsts)),
          InductionHypotheses[VeritasConstruct](welltypedLookupBindIH))))

    }


  }

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

  object filterRowsPreservesTableInduction extends MockInduction(MetaVar("rt")) {
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasConstruct],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val filterRowsPreservesTableTemptyObl = produce.newObligation(fullSQLspec, filterRowsPreservesTableTempty)
      val filterRowsPreservesTableTconsObl = produce.newObligation(fullSQLspec, filterRowsPreservesTableTcons)

      Seq((filterRowsPreservesTableTemptyObl, StructInductCase[VeritasConstruct](filterRowsPreservesTableTempty.goals.head.name,
        None,
        InductionHypotheses[VeritasConstruct](Axioms(Seq())))),
        (filterRowsPreservesTableTconsObl, StructInductCase[VeritasConstruct](filterRowsPreservesTableTcons.goals.head.name,
          Some(FixedVars(filterRowsPreservesTableTconsConsts)),
          InductionHypotheses[VeritasConstruct](filterRowsPreservesTableTconsIH))))

    }
  }


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
      ).===>("projectCols-progress-acons-IH")(
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

  object projectColsProgressInduction extends MockInduction(MetaVar("al2")) {
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasConstruct],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val projectColsProgressAemptyObl = produce.newObligation(fullSQLspec, projectColsProgressAempty)
      val projectColsProgressAconsObl = produce.newObligation(fullSQLspec, projectColsProgressAcons)

      Seq((projectColsProgressAemptyObl, StructInductCase[VeritasConstruct](projectColsProgressAempty.goals.head.name,
        None,
        InductionHypotheses[VeritasConstruct](Axioms(Seq())))),
        (projectColsProgressAconsObl, StructInductCase[VeritasConstruct](projectColsProgressAcons.goals.head.name,
          Some(FixedVars(projectColsProgressAconsConsts)),
          InductionHypotheses[VeritasConstruct](projectColsProgressAconsIH))))

    }
  }

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


  object projectTypeImpliesFindColInduction extends MockInduction(MetaVar("al2")) {
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasConstruct],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val projectTypeImpliesFindColAemptyObl = produce.newObligation(fullSQLspec, projectTypeImpliesFindColAempty)
      val projectTypeImpliesFindColAconsObl = produce.newObligation(fullSQLspec, projectTypeImpliesFindColAcons)

      Seq((projectTypeImpliesFindColAemptyObl, StructInductCase[VeritasConstruct](projectTypeImpliesFindColAempty.goals.head.name,
        None,
        InductionHypotheses[VeritasConstruct](Axioms(Seq())))),
        (projectTypeImpliesFindColAconsObl, StructInductCase[VeritasConstruct](projectTypeImpliesFindColAcons.goals.head.name,
          Some(FixedVars(projectTypeImpliesFindColAconsConsts)),
          InductionHypotheses[VeritasConstruct](projectTypeImpliesFindColAconsIH))))

    }
  }

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

  val findColTypeImpliesfindColAcons: Goals = goal(
    ((~'al === 'acons (~'a, 'alr)) &
      ('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('findColType (~'n, ~'tt) === 'someFType (~'ft))
      ).===>("findColType-implies-findCol-acons")(
      exists(~'rt) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt)
    )) //requires lemma dropFirstColRawPreservesWelltypedRaw

  object findColTypeImpliesfindColInduction extends MockInduction(MetaVar("al")) {
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasConstruct],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val findColTypeImpliesfindColAemptyObl = produce.newObligation(fullSQLspec, findColTypeImpliesfindColAempty)
      val findColTypeImpliesfindColAconsObl = produce.newObligation(fullSQLspec, findColTypeImpliesfindColAcons)

      Seq((findColTypeImpliesfindColAemptyObl, StructInductCase[VeritasConstruct](findColTypeImpliesfindColAempty.goals.head.name,
        None,
        InductionHypotheses[VeritasConstruct](Axioms(Seq())))),
        (findColTypeImpliesfindColAconsObl, StructInductCase[VeritasConstruct](findColTypeImpliesfindColAcons.goals.head.name,
          Some(FixedVars(findColTypeImpliesfindColAconsConsts)),
          InductionHypotheses[VeritasConstruct](findColTypeImpliesfindColAconsIH))))

    }
  }

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


  //end of induction cases projectTypeAttrLImpliesfindAllColType


  object projectTypeAttrLImpliesfindAllColTypeInduction extends MockInduction(MetaVar("al")) {
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasConstruct],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val projectTypeAttrLImpliesfindAllColTypeAemptyObl = produce.newObligation(fullSQLspec, projectTypeAttrLImpliesfindAllColTypeAempty)
      val projectTypeAttrLImpliesfindAllColTypeAconsObl = produce.newObligation(fullSQLspec, projectTypeAttrLImpliesfindAllColTypeAcons)

      Seq((projectTypeAttrLImpliesfindAllColTypeAemptyObl, StructInductCase[VeritasConstruct](projectTypeAttrLImpliesfindAllColTypeAempty.goals.head.name,
        None,
        InductionHypotheses[VeritasConstruct](Axioms(Seq())))),
        (projectTypeAttrLImpliesfindAllColTypeAconsObl, StructInductCase[VeritasConstruct](projectTypeAttrLImpliesfindAllColTypeAcons.goals.head.name,
          Some(FixedVars(projectTypeAttrLImpliesfindAllColTypeAconsConsts)),
          InductionHypotheses[VeritasConstruct](projectTypeAttrLImpliesfindAllColTypeAconsIH))))

    }
  }

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


  object dropFirstColRawPreservesWelltypedRawInduction extends MockInduction(MetaVar("rt")) {
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasConstruct],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasConstruct, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val dropFirstColRawPreservesWelltypedRawTemptyObl = produce.newObligation(fullSQLspec, dropFirstColRawPreservesWelltypedRawTempty)
      val dropFirstColRawPreservesWelltypedRawTconsObl = produce.newObligation(fullSQLspec, dropFirstColRawPreservesWelltypedRawTcons)

      Seq((dropFirstColRawPreservesWelltypedRawTemptyObl, StructInductCase[VeritasConstruct](dropFirstColRawPreservesWelltypedRawTempty.goals.head.name,
        None,
        InductionHypotheses[VeritasConstruct](Axioms(Seq())))),
        (dropFirstColRawPreservesWelltypedRawTconsObl, StructInductCase[VeritasConstruct](dropFirstColRawPreservesWelltypedRawTcons.goals.head.name,
          Some(FixedVars(dropFirstColRawPreservesWelltypedRawTconsConsts)),
          InductionHypotheses[VeritasConstruct](dropFirstColRawPreservesWelltypedRawTconsIH))))

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

      //note: a real tactic would have to extract the information to be propagated
      // in the edges from the given obllabels (unused here) and decide which one
      // to forward where
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
  PropertyTypes.registerPropertyType[VerifierFailure[_, _]](g.store)
  PropertyTypes.registerPropertyType[TSTPProof](g.store)
  PropertyTypes.registerPropertyType[MockLemmaApplication](g.store)
  PropertyTypes.registerPropertyType[LemmaApplication[_]](g.store)
  PropertyTypes.registerPropertyType[successfulLookupInduction.type](g.store)
  PropertyTypes.registerPropertyType[welltypedLookupInduction.type](g.store)
  PropertyTypes.registerPropertyType[filterRowsPreservesTableInduction.type](g.store)
  PropertyTypes.registerPropertyType[projectColsProgressInduction.type](g.store)
  PropertyTypes.registerPropertyType[projectTypeImpliesFindColInduction.type](g.store)
  PropertyTypes.registerPropertyType[findColTypeImpliesfindColInduction.type](g.store)
  PropertyTypes.registerPropertyType[projectTypeAttrLImpliesfindAllColTypeInduction.type](g.store)
  PropertyTypes.registerPropertyType[dropFirstColRawPreservesWelltypedRawInduction.type](g.store)


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


  test("Timeout for individual set cases (SQL progress proof), using Vampire 4.1, 1 sec") {
    val simpleVerifier = new TPTPVampireVerifier(1)

    for (ps <- setPS) {
      val result = g.verifyProofStep(ps, simpleVerifier)

      assert(result.status.isInstanceOf[Finished[_, _]])
      assert(result.errorMsg.nonEmpty)
      assert(result.errorMsg.get == "Time limit")
    }
  }

  //  test("Proving a single set case") {
  //    val simpleVerifier = new TPTPVampireVerifier(30, "4.0")
  //
  //    val result = g.verifyProofStep(setPS.head, simpleVerifier)
  //
  //    println(result.status)
  //    assert(result.status.isInstanceOf[Finished[_, _]])
  //    assert(result.status.isVerified)
  //  }
  //
  //  test("Proving all individual set cases with Vampire 4.0") {
  //    val simpleVerifier = new TPTPVampireVerifier(30, "4.0")
  //
  //    for (ps <- setPS) {
  //      val result = g.verifyProofStep(ps, simpleVerifier)
  //
  //      assert(result.status.isInstanceOf[Finished[_, _]])
  //      assert(result.status.isVerified)
  //      assert(result.errorMsg.isEmpty)
  //      assert(result.evidence.nonEmpty)
  //    }
  //  }

  test("Proving Case Distinction steps, Vampire 4.1, 5 seconds") {
    val simpleVerifier = new TPTPVampireVerifier(5)

    val res1 = g.verifyProofStep(unioncasePS, simpleVerifier)
    val res2 = g.verifyProofStep(intersectioncasePS, simpleVerifier)
    val res3 = g.verifyProofStep(differencecasePS, simpleVerifier)


    assert(res1.status.isInstanceOf[Finished[_, _]])
    assert(res1.status.isVerified)
    assert(res1.errorMsg.isEmpty)
    assert(res1.evidence.nonEmpty)

    assert(res2.status.isInstanceOf[Finished[_, _]])
    assert(res2.status.isVerified)
    assert(res2.errorMsg.isEmpty)
    assert(res2.evidence.nonEmpty)

    assert(res3.status.isInstanceOf[Finished[_, _]])
    assert(res3.status.isVerified)
    assert(res3.errorMsg.isEmpty)
    assert(res3.evidence.nonEmpty)
  }


  // here, the SQL lemmas necessary for progress (selectFromWhere case) start

  val successfulLookup: Lemmas = lemma(
    ('StoreContextConsistent (~'TS, ~'TTC) &
      ('lookupContext (~'tn, ~'TTC) === 'someTType (~'tt))
      ).===>("successful-lookup")(
      exists(~'t) |
        ('lookupStore (~'tn, ~'TS) === 'someTable (~'t)))
  )

  val welltypedLookup: Lemmas = lemma(
    ('StoreContextConsistent (~'TS, ~'TTC) &
      ('lookupStore (~'tn, ~'TS) === 'someTable (~'t)) &
      ('lookupContext (~'tn, ~'TTC) === 'someTType (~'tt))
      ).===>("welltyped-lookup")(
      'welltypedtable (~'tt, ~'t))
  )

  val filterPreservesType: Lemmas = lemma(
    ('welltypedtable (~'tt, ~'t)
      ).===>("filter-preserves-type")(
      'welltypedtable (~'tt, 'filterTable (~'t, ~'p)))
  ) //proved directly via filterRowsPreservesTable

  val projectTableProgress: Lemmas = lemma(
    ('welltypedtable (~'tt, ~'t) &
      ('projectType (~'s, ~'tt) === 'someTType (~'tt2))
      ).===>("projectTable-progress")(
      exists(~'t2) |
        'projectTable (~'s, ~'t) === 'someTable (~'t2))
  ) //proof by case distinction on 's (maybe not necessary?); list case by projectColsProgress

  val selcase = MockInduction.selectCase(SQLProgressTselectFromWhere.goals.head.name,
    g.requiredObls(rootinductionPS))


  //apply lemma application tactic to selection case
  val selLemmaTac = MockLemmaApplication(Seq(successfulLookup, welltypedLookup,
    filterPreservesType, projectTableProgress))
  val selLemmaPS = g.applyTactic(selcase, selLemmaTac)

  test("Verify lemma application step (inconclusive)") {
    val simpleVerifier = new TPTPVampireVerifier(3)

    val result = g.verifyProofStep(selLemmaPS, simpleVerifier)

    //println(result.status)
    assert(result.status.isInstanceOf[Finished[_, _]])

  }

  val successfulLookupobl = MockLemmaApplication.selectLemma(successfulLookup.lemmas.head.name,
    g.requiredObls(selLemmaPS))

  val successfulLookupPS = g.applyTactic(successfulLookupobl, successfulLookupInduction)

  val successfulLookupbasecase = MockInduction.selectCase(successfulLookupEmpty.goals.head.name, g.requiredObls(successfulLookupPS))
  val successfulLookupbasecasePL = g.applyTactic(successfulLookupbasecase, Solve[VeritasConstruct, VeritasConstruct])

  val successfulLookupstepcase = MockInduction.selectCase(successfulLookupBind.goals.head.name, g.requiredObls(successfulLookupPS))
  val successfulLookupstepcasePL = g.applyTactic(successfulLookupstepcase, Solve[VeritasConstruct, VeritasConstruct])

  test("Verify cases of successfulLookup (Vampire 4.1)") {
    val simpleVerifier = new TPTPVampireVerifier(5)

    val resbase = g.verifyProofStep(successfulLookupbasecasePL, simpleVerifier)
    val resstep = g.verifyProofStep(successfulLookupstepcasePL, simpleVerifier)

    assert(resbase.status.isInstanceOf[Finished[_, _]])
    assert(resbase.status.isVerified)
    assert(resbase.errorMsg.isEmpty)
    assert(resbase.evidence.nonEmpty)

    assert(resstep.status.isInstanceOf[Finished[_, _]])
    assert(resstep.status.isVerified)
    assert(resstep.errorMsg.isEmpty)
    assert(resstep.evidence.nonEmpty)

  }

  val welltypedLookupobl = MockLemmaApplication.selectLemma(welltypedLookup.lemmas.head.name,
    g.requiredObls(selLemmaPS))

  val welltypedLookupPS = g.applyTactic(welltypedLookupobl, welltypedLookupInduction)

  val welltypedLookupbasecase = MockInduction.selectCase(welltypedLookupEmpty.goals.head.name, g.requiredObls(welltypedLookupPS))
  val welltypedLookupbasecasePS = g.applyTactic(welltypedLookupbasecase, Solve[VeritasConstruct, VeritasConstruct])

  val welltypedLookupstepcase = MockInduction.selectCase(welltypedLookupBind.goals.head.name, g.requiredObls(welltypedLookupPS))
  val welltypedLookupstepcasePS = g.applyTactic(welltypedLookupstepcase, Solve[VeritasConstruct, VeritasConstruct])


  test("Verify cases of welltypedLookup (Vampire 4.1)") {
    val simpleVerifier = new TPTPVampireVerifier(5)

    val resbase = g.verifyProofStep(welltypedLookupbasecasePS, simpleVerifier)
    val resstep = g.verifyProofStep(welltypedLookupstepcasePS, simpleVerifier)

    assert(resbase.status.isInstanceOf[Finished[_, _]])
    assert(resbase.status.isVerified)
    assert(resbase.errorMsg.isEmpty)
    assert(resbase.evidence.nonEmpty)

    assert(resstep.status.isInstanceOf[Finished[_, _]])
    assert(resstep.status.isVerified)
    assert(resstep.errorMsg.isEmpty)
    assert(resstep.evidence.nonEmpty)

  }


  val filterRowsPreservesTable: Lemmas = lemma(
    ('welltypedRawtable (~'tt, ~'rt)
      ).===>("filterRows-preserves-table")(
      'welltypedRawtable (~'tt, 'filterRows (~'rt, ~'al, ~'p))
    ))


  val filterPreservesTypeobl = MockLemmaApplication.selectLemma(filterPreservesType.lemmas.head.name,
    g.requiredObls(selLemmaPS))

  val filterPreservesTypePS = g.applyTactic(filterPreservesTypeobl,
    MockLemmaApplication(Seq(filterRowsPreservesTable)))

  test("Verify filterPreservesType via auxiliary lemma (Vampire 4.1)") {
    val simpleVerifier = new TPTPVampireVerifier(5)

    val result = g.verifyProofStep(filterPreservesTypePS, simpleVerifier)

    assert(result.status.isInstanceOf[Finished[_, _]])
    assert(result.status.isVerified)
    assert(result.errorMsg.isEmpty)
    assert(result.evidence.nonEmpty)

  }


  val filterRowsPreservesTableObl = MockLemmaApplication.selectLemma(filterRowsPreservesTable.lemmas.head.name,
    g.requiredObls(filterPreservesTypePS))

  val filterRowsPreservesTableOblPS = g.applyTactic(filterRowsPreservesTableObl, filterRowsPreservesTableInduction)

  val filterRowsPreservesTablebasecase = MockInduction.selectCase(filterRowsPreservesTableTempty.goals.head.name,
    g.requiredObls(filterRowsPreservesTableOblPS))
  val filterRowsPreservesTablebasecasePS = g.applyTactic(filterRowsPreservesTablebasecase,
    Solve[VeritasConstruct, VeritasConstruct])

  val filterRowsPreservesTablestepcase = MockInduction.selectCase(filterRowsPreservesTableTcons.goals.head.name, g.requiredObls(filterRowsPreservesTableOblPS))
  val filterRowsPreservesTablestepcasePS = g.applyTactic(filterRowsPreservesTablestepcase,
    Solve[VeritasConstruct, VeritasConstruct])


  test("Verify cases of filterRowsPreservesTable (Vampire 4.1)") {
    val simpleVerifier = new TPTPVampireVerifier(5)

    val resbase = g.verifyProofStep(filterRowsPreservesTablebasecasePS, simpleVerifier)
    val resstep = g.verifyProofStep(filterRowsPreservesTablestepcasePS, simpleVerifier)

    assert(resbase.status.isInstanceOf[Finished[_, _]])
    assert(resbase.status.isVerified)
    assert(resbase.errorMsg.isEmpty)
    assert(resbase.evidence.nonEmpty)

    assert(resstep.status.isInstanceOf[Finished[_, _]])
    assert(resstep.status.isVerified)
    assert(resstep.errorMsg.isEmpty)
    assert(resstep.evidence.nonEmpty)

  }

  val projectColsProgress: Lemmas = lemma(
    (('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('projectType ('list (~'al2), ~'tt) === 'someTType (~'tt2))
      ).===>("projectCols-progress")(
      exists(~'rt2) |
        'projectCols (~'al2, ~'al, ~'rt) === 'someRawTable (~'rt2))
  )

  //try to prove projectTableProgress via lemma application with projectColsProgress?
  //yes, works, apparently no case distinction necessary!
  val projectTableProgressobl = MockLemmaApplication.selectLemma(projectTableProgress.lemmas.head.name,
    g.requiredObls(selLemmaPS))

  val projectTableProgressPS = g.applyTactic(projectTableProgressobl,
    MockLemmaApplication(Seq(projectColsProgress)))

  test("Verify projectTableProgress via auxiliary lemma (Vampire 4.1)") {
    val simpleVerifier = new TPTPVampireVerifier(20)

    val result = g.verifyProofStep(projectTableProgressPS, simpleVerifier)

    println(result.status)
    assert(result.status.isInstanceOf[Finished[_, _]])
    assert(result.status.isVerified)
    assert(result.errorMsg.isEmpty)
    assert(result.evidence.nonEmpty)

  }

  val projectColsProgressObl = MockLemmaApplication.selectLemma(projectColsProgress.lemmas.head.name,
    g.requiredObls(projectTableProgressPS))

  val projectColsProgressPS = g.applyTactic(projectColsProgressObl, projectColsProgressInduction)

  val projectColsProgressbasecase = MockInduction.selectCase(projectColsProgressAempty.goals.head.name,
    g.requiredObls(projectColsProgressPS))
  val projectColsProgressbasecasePS = g.applyTactic(projectColsProgressbasecase,
    Solve[VeritasConstruct, VeritasConstruct])

  val projectTypeImpliesFindCol: Lemmas = lemma(
    (('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('projectTypeAttrL (~'al2, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al2))
      ).===>("projectType-implies-findCol")(
      exists(~'rt2) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt2))
  )

  val projectColsProgressstepcase = MockInduction.selectCase(projectColsProgressAcons.goals.head.name, g.requiredObls(projectColsProgressPS))
  val projectColsProgressstepcasePS = g.applyTactic(projectColsProgressstepcase,
    MockLemmaApplication(Seq(projectTypeImpliesFindCol)))


  test("Verify cases of projectColsProgress (Vampire 4.1)") {
    val simpleVerifier = new TPTPVampireVerifier(5)

    val resbase = g.verifyProofStep(projectColsProgressbasecasePS, simpleVerifier)
    val resstep = g.verifyProofStep(projectColsProgressstepcasePS, simpleVerifier)

    assert(resbase.status.isInstanceOf[Finished[_, _]])
    assert(resbase.status.isVerified)
    assert(resbase.errorMsg.isEmpty)
    assert(resbase.evidence.nonEmpty)

    assert(resstep.status.isInstanceOf[Finished[_, _]])
    assert(resstep.status.isVerified)
    assert(resstep.errorMsg.isEmpty)
    assert(resstep.evidence.nonEmpty)

  }

  val projectTypeImpliesFindColObl = MockLemmaApplication.selectLemma(projectTypeImpliesFindCol.lemmas.head.name,
    g.requiredObls(projectColsProgressstepcasePS))

  val projectTypeImpliesFindColPS = g.applyTactic(projectTypeImpliesFindColObl, projectTypeImpliesFindColInduction)

  val projectTypeImpliesFindColbasecase = MockInduction.selectCase(projectTypeImpliesFindColAempty.goals.head.name,
    g.requiredObls(projectTypeImpliesFindColPS ))
  val projectTypeImpliesFindColbasecasePS = g.applyTactic(projectTypeImpliesFindColbasecase,
    Solve[VeritasConstruct, VeritasConstruct])

  val findColTypeImpliesfindCol: Lemmas = lemma(
    (('welltypedtable (~'tt, 'table (~'al, ~'rt))) &
      ('findColType (~'n, ~'tt) === 'someFType (~'ft))
      ).===>("findColType-implies-findCol")(
      exists(~'rt) |
        'findCol (~'n, ~'al, ~'rt) === 'someRawTable (~'rt)
    ))

  val projectTypeAttrLImpliesfindAllColType: Lemmas = lemma(
    (('projectTypeAttrL (~'al, ~'tt) === 'someTType (~'tt2)) &
      ('attrIn (~'n, ~'al))
      ).===>("projectTypeAttrL-implies-findAllColType")(
      exists(~'ft) |
        'findColType (~'n, ~'tt) === 'someFType (~'ft)
    ))

  val projectTypeImpliesFindColstepcase = MockInduction.selectCase(projectTypeImpliesFindColAcons.goals.head.name, g.requiredObls(projectTypeImpliesFindColPS))
  val projectTypeImpliesFindColstepcasePS = g.applyTactic(projectTypeImpliesFindColstepcase,
    MockLemmaApplication(Seq(findColTypeImpliesfindCol, projectTypeAttrLImpliesfindAllColType)))


  val dropFirstColRawPreservesWelltypedRaw: Lemmas = lemma(
    ((~'tt === 'ttcons (~'n, ~'ft, ~'ttr)) &
      ('welltypedRawtable (~'tt, ~'rt))
      ).===>("dropFirstColRaw-preserves-welltypedRaw")(
      'welltypedRawtable (~'ttr, 'dropFirstColRaw (~'rt))
    ))


}
