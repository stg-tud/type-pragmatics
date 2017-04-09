package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.SQLMockTactics._
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic._
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{Finished, TPTPVampireVerifier, TSTPProof, VerifierFailure}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}
import org.scalatest.FunSuite

object SQLMockTactics {
  //This object contains all MockTactics and hand-coded obligations for the SQL soundness proof graph

  import SQLSoundnessProofSteps._

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


      Seq((tvaluecase, StructInductCase[VeritasConstruct](SQLProgressTtvalue.goals.head.name,
        None,
        InductionHypotheses[VeritasConstruct](Axioms(Seq())))),
        (selectfromwherecase, StructInductCase[VeritasConstruct](SQLProgressTselectFromWhere.goals.head.name,
          None,
          InductionHypotheses[VeritasConstruct](Axioms(Seq())))),
        (unioncase, StructInductCase[VeritasConstruct](SQLProgressTUnion.goals.head.name,
          Some(FixedVars(unionconsts)),
          InductionHypotheses[VeritasConstruct](Axioms(Seq(SQLProgressTUnionIH1.axioms.head, SQLProgressTUnionIH2.axioms.head))))),
        (intersectioncase, StructInductCase[VeritasConstruct](SQLProgressTIntersection.goals.head.name,
          Some(FixedVars(intersectionconsts)),
          InductionHypotheses[VeritasConstruct](Axioms(Seq(SQLProgressTIntersectionIH1.axioms.head, SQLProgressTIntersectionIH2.axioms.head))))),
        (differencecase, StructInductCase[VeritasConstruct](SQLProgressTDifference.goals.head.name,
          Some(FixedVars(differenceconsts)),
          InductionHypotheses[VeritasConstruct](Axioms(Seq(SQLProgressTDifferenceIH1.axioms.head, SQLProgressTDifferenceIH2.axioms.head))))))
    }
  }

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
          InductionHypotheses[VeritasConstruct](mkSQLProgressTSetCaseIH(2, setname, 'q2)))),
        (mkcase(2), CaseDistinctionCase[VeritasConstruct](setname + "3",
          Some(FixedVars(setconsts)),
          InductionHypotheses[VeritasConstruct](mkSQLProgressTSetCaseIH(1, setname, 'q1)))))
    }

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

}


// Constructing the SQL soundness proof graph
// We instantiate S = VeritasConstruct (should be a Module) and P = VeritasConstruct (Should be Goal/local)
class SQLSoundnessProofGraph(file: File) {

  import SQLMockTactics._
  import SQLSoundnessProofSteps._

  val g: ProofGraphXodus[VeritasConstruct, VeritasConstruct] =
    new ProofGraphXodus[VeritasConstruct, VeritasConstruct](file)
  SQLSoundnessProofGraph.initializeGraphTypes(g)


  //progress root obligation
  val progressObligation: g.Obligation = g.newObligation(fullSQLspec, SQLProgress)
  g.storeObligation("SQL progress", progressObligation)

  // first proof step: structural induction
  val rootinductionPS: g.ProofStep = g.applyTactic(progressObligation, rootInductionProgress)

  //apply simply Solve-tactic to t-value base case
  val tvaluecaseobl = MockInduction.selectCase(SQLProgressTtvalue.goals.head.name, g.requiredObls(rootinductionPS))
  val tvaluecasePS = g.applyTactic(tvaluecaseobl, Solve[VeritasConstruct, VeritasConstruct])

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

  //apply Solve tactic to all of the set cases
  val setobls = g.requiredObls(unioncasePS) ++
    g.requiredObls(intersectioncasePS) ++ g.requiredObls(differencecasePS)

  val setPS = for ((o, e) <- setobls) yield {
    g.applyTactic(o, Solve[VeritasConstruct, VeritasConstruct])
  }

  //prove selectFromWhereCase via auxiliary lemmas:
  val selcase = MockInduction.selectCase(SQLProgressTselectFromWhere.goals.head.name,
    g.requiredObls(rootinductionPS))


  //apply lemma application tactic to selection case
  val selLemmaTac = MockLemmaApplication(Seq(successfulLookup, welltypedLookup,
    filterPreservesType, projectTableProgress))
  val selLemmaPS = g.applyTactic(selcase, selLemmaTac)

  val successfulLookupobl = MockLemmaApplication.selectLemma(successfulLookup.lemmas.head.name,
    g.requiredObls(selLemmaPS))

  // prove lemma successfulLookup via simple structural induction
  val successfulLookupPS = g.applyTactic(successfulLookupobl, successfulLookupInduction)

  val successfulLookupbasecase = MockInduction.selectCase(successfulLookupEmpty.goals.head.name, g.requiredObls(successfulLookupPS))
  val successfulLookupbasecasePL = g.applyTactic(successfulLookupbasecase, Solve[VeritasConstruct, VeritasConstruct])

  val successfulLookupstepcase = MockInduction.selectCase(successfulLookupBind.goals.head.name, g.requiredObls(successfulLookupPS))
  val successfulLookupstepcasePL = g.applyTactic(successfulLookupstepcase, Solve[VeritasConstruct, VeritasConstruct])

  val welltypedLookupobl = MockLemmaApplication.selectLemma(welltypedLookup.lemmas.head.name,
    g.requiredObls(selLemmaPS))

  // prove lemma welltypedLookup via simple structural induction
  val welltypedLookupPS = g.applyTactic(welltypedLookupobl, welltypedLookupInduction)

  val welltypedLookupbasecase = MockInduction.selectCase(welltypedLookupEmpty.goals.head.name, g.requiredObls(welltypedLookupPS))
  val welltypedLookupbasecasePS = g.applyTactic(welltypedLookupbasecase, Solve[VeritasConstruct, VeritasConstruct])

  val welltypedLookupstepcase = MockInduction.selectCase(welltypedLookupBind.goals.head.name, g.requiredObls(welltypedLookupPS))
  val welltypedLookupstepcasePS = g.applyTactic(welltypedLookupstepcase, Solve[VeritasConstruct, VeritasConstruct])

  // prove lemma filterPreservesType via auxiliary lemma filterRowsPreservesTable
  val filterPreservesTypeobl = MockLemmaApplication.selectLemma(filterPreservesType.lemmas.head.name,
    g.requiredObls(selLemmaPS))

  val filterPreservesTypePS = g.applyTactic(filterPreservesTypeobl,
    MockLemmaApplication(Seq(filterRowsPreservesTable)))

  val filterRowsPreservesTableObl = MockLemmaApplication.selectLemma(filterRowsPreservesTable.lemmas.head.name,
    g.requiredObls(filterPreservesTypePS))

  val filterRowsPreservesTableOblPS = g.applyTactic(filterRowsPreservesTableObl, filterRowsPreservesTableInduction)

  // prove lemmma filterRowsPreservesTable via simple structural induction
  val filterRowsPreservesTablebasecase = MockInduction.selectCase(filterRowsPreservesTableTempty.goals.head.name,
    g.requiredObls(filterRowsPreservesTableOblPS))
  val filterRowsPreservesTablebasecasePS = g.applyTactic(filterRowsPreservesTablebasecase,
    Solve[VeritasConstruct, VeritasConstruct])

  val filterRowsPreservesTablestepcase = MockInduction.selectCase(filterRowsPreservesTableTcons.goals.head.name, g.requiredObls(filterRowsPreservesTableOblPS))
  val filterRowsPreservesTablestepcasePS = g.applyTactic(filterRowsPreservesTablestepcase,
    Solve[VeritasConstruct, VeritasConstruct])

  //try to prove projectTableProgress via lemma application with projectColsProgress?
  //yes, works, apparently no case distinction necessary!
  val projectTableProgressobl = MockLemmaApplication.selectLemma(projectTableProgress.lemmas.head.name,
    g.requiredObls(selLemmaPS))

  val projectTableProgressPS = g.applyTactic(projectTableProgressobl,
    MockLemmaApplication(Seq(projectColsProgress)))

  //prove projectColsProgress via induction
  val projectColsProgressObl = MockLemmaApplication.selectLemma(projectColsProgress.lemmas.head.name,
    g.requiredObls(projectTableProgressPS))

  val projectColsProgressPS = g.applyTactic(projectColsProgressObl, projectColsProgressInduction)

  val projectColsProgressbasecase = MockInduction.selectCase(projectColsProgressAempty.goals.head.name,
    g.requiredObls(projectColsProgressPS))
  val projectColsProgressbasecasePS = g.applyTactic(projectColsProgressbasecase,
    Solve[VeritasConstruct, VeritasConstruct])

  // step case requires an auxiliary lemma (projectTypeImpliesFindCol)
  val projectColsProgressstepcase = MockInduction.selectCase(projectColsProgressAcons.goals.head.name, g.requiredObls(projectColsProgressPS))
  val projectColsProgressstepcasePS = g.applyTactic(projectColsProgressstepcase,
    MockLemmaApplication(Seq(projectTypeImpliesFindCol)))

  //prove projectTypeImpliesFindCol via induction
  val projectTypeImpliesFindColObl = MockLemmaApplication.selectLemma(projectTypeImpliesFindCol.lemmas.head.name,
    g.requiredObls(projectColsProgressstepcasePS))

  val projectTypeImpliesFindColPS = g.applyTactic(projectTypeImpliesFindColObl, projectTypeImpliesFindColInduction)

  val projectTypeImpliesFindColbasecase = MockInduction.selectCase(projectTypeImpliesFindColAempty.goals.head.name,
    g.requiredObls(projectTypeImpliesFindColPS))
  val projectTypeImpliesFindColbasecasePS = g.applyTactic(projectTypeImpliesFindColbasecase,
    Solve[VeritasConstruct, VeritasConstruct])

  // step case requires two auxiliary lemmas
  val projectTypeImpliesFindColstepcase = MockInduction.selectCase(projectTypeImpliesFindColAcons.goals.head.name,
    g.requiredObls(projectTypeImpliesFindColPS))
  val projectTypeImpliesFindColstepcasePS = g.applyTactic(projectTypeImpliesFindColstepcase,
    MockLemmaApplication(Seq(findColTypeImpliesfindCol, projectTypeAttrLImpliesfindAllColType)))

  //prove findColTypeImpliesfindCol via induction
  val findColTypeImpliesfindColObl = MockLemmaApplication.selectLemma(findColTypeImpliesfindCol.lemmas.head.name,
    g.requiredObls(projectTypeImpliesFindColstepcasePS))

  val findColTypeImpliesfindColPS = g.applyTactic(findColTypeImpliesfindColObl, findColTypeImpliesfindColInduction)

  val findColTypeImpliesfindColbasecase = MockInduction.selectCase(findColTypeImpliesfindColAempty.goals.head.name,
    g.requiredObls(findColTypeImpliesfindColPS))
  val findColTypeImpliesfindColbasecasePS = g.applyTactic(findColTypeImpliesfindColbasecase,
    Solve[VeritasConstruct, VeritasConstruct])

  //step requires auxiliary lemma dropFirstColRawPreservesWelltypedRaw
  val findColTypeImpliesfindColstepcase = MockInduction.selectCase(findColTypeImpliesfindColAcons.goals.head.name,
    g.requiredObls(findColTypeImpliesfindColPS))
  val findColTypeImpliesfindColstepcasePS = g.applyTactic(findColTypeImpliesfindColstepcase,
    MockLemmaApplication(Seq(dropFirstColRawPreservesWelltypedRaw)))

  //prove projectTypeAttrLImpliesfindAllColType via induction
  val projectTypeAttrLImpliesfindAllColTypeObl = MockLemmaApplication.selectLemma(projectTypeAttrLImpliesfindAllColType.lemmas.head.name,
    g.requiredObls(projectTypeImpliesFindColstepcasePS))

  val projectTypeAttrLImpliesfindAllColTypePS = g.applyTactic(projectTypeAttrLImpliesfindAllColTypeObl, projectTypeAttrLImpliesfindAllColTypeInduction)

  val projectTypeAttrLImpliesfindAllColTypebasecase = MockInduction.selectCase(projectTypeAttrLImpliesfindAllColTypeAempty.goals.head.name,
    g.requiredObls(projectTypeAttrLImpliesfindAllColTypePS))
  val projectTypeAttrLImpliesfindAllColTypebasecasePS = g.applyTactic(projectTypeAttrLImpliesfindAllColTypebasecase,
    Solve[VeritasConstruct, VeritasConstruct])

  val projectTypeAttrLImpliesfindAllColTypestepcase = MockInduction.selectCase(projectTypeAttrLImpliesfindAllColTypeAcons.goals.head.name,
    g.requiredObls(projectTypeAttrLImpliesfindAllColTypePS))
  val projectTypeAttrLImpliesfindAllColTypestepcasePS = g.applyTactic(projectTypeAttrLImpliesfindAllColTypestepcase,
    Solve[VeritasConstruct, VeritasConstruct])

  //prove dropFirstColRawPreservesWelltypedRaw via induction
  val dropFirstColRawPreservesWelltypedRawObl = MockLemmaApplication.selectLemma(dropFirstColRawPreservesWelltypedRaw.lemmas.head.name,
    g.requiredObls(findColTypeImpliesfindColstepcasePS))

  val dropFirstColRawPreservesWelltypedRawPS = g.applyTactic(dropFirstColRawPreservesWelltypedRawObl, dropFirstColRawPreservesWelltypedRawInduction)

  val dropFirstColRawPreservesWelltypedRawbasecase = MockInduction.selectCase(dropFirstColRawPreservesWelltypedRawTempty.goals.head.name,
    g.requiredObls(dropFirstColRawPreservesWelltypedRawPS))
  val dropFirstColRawPreservesWelltypedRawbasecasePS = g.applyTactic(dropFirstColRawPreservesWelltypedRawbasecase,
    Solve[VeritasConstruct, VeritasConstruct])

  val dropFirstColRawPreservesWelltypedRawstepcase = MockInduction.selectCase(dropFirstColRawPreservesWelltypedRawTcons.goals.head.name,
    g.requiredObls(dropFirstColRawPreservesWelltypedRawPS))
  val dropFirstColRawPreservesWelltypedRawstepcasePS = g.applyTactic(dropFirstColRawPreservesWelltypedRawstepcase,
    Solve[VeritasConstruct, VeritasConstruct])

  //verify chosen steps with chosen verifiers
  def verifySingleStepsSimple() = {
    val simpleVampire4_1 = new TPTPVampireVerifier(5)
    val simpleVampire4_1_20 = new TPTPVampireVerifier(20)

    g.verifyProofStep(tvaluecasePS, simpleVampire4_1)

    //verify case distinction steps
    g.verifyProofStep(unioncasePS, simpleVampire4_1)
    g.verifyProofStep(intersectioncasePS, simpleVampire4_1)
    g.verifyProofStep(differencecasePS, simpleVampire4_1)


    //verify the individual set cases (inconclusive)
    for (ps <- setPS) {
      g.verifyProofStep(ps, simpleVampire4_1)
    }

    //Inconclusive step
    g.verifyProofStep(selLemmaPS, simpleVampire4_1)

    //successful steps (?)
    g.verifyProofStep(successfulLookupbasecasePL, simpleVampire4_1)
    g.verifyProofStep(successfulLookupstepcasePL, simpleVampire4_1)

    g.verifyProofStep(welltypedLookupbasecasePS, simpleVampire4_1)
    g.verifyProofStep(welltypedLookupstepcasePS, simpleVampire4_1)

    g.verifyProofStep(filterPreservesTypePS, simpleVampire4_1)

    g.verifyProofStep(filterRowsPreservesTablebasecasePS, simpleVampire4_1)
    g.verifyProofStep(filterRowsPreservesTablestepcasePS, simpleVampire4_1)

    g.verifyProofStep(projectTableProgressPS, simpleVampire4_1_20)

    g.verifyProofStep(projectColsProgressbasecasePS, simpleVampire4_1)
    g.verifyProofStep(projectColsProgressstepcasePS, simpleVampire4_1)

    g.verifyProofStep(projectTypeImpliesFindColbasecasePS, simpleVampire4_1)
    g.verifyProofStep(projectTypeImpliesFindColstepcasePS, simpleVampire4_1)

    g.verifyProofStep(findColTypeImpliesfindColbasecasePS, simpleVampire4_1)
    g.verifyProofStep(findColTypeImpliesfindColstepcasePS, simpleVampire4_1)

    g.verifyProofStep(projectTypeAttrLImpliesfindAllColTypebasecasePS, simpleVampire4_1)
    g.verifyProofStep(projectTypeAttrLImpliesfindAllColTypestepcasePS, simpleVampire4_1)

    g.verifyProofStep(dropFirstColRawPreservesWelltypedRawbasecasePS, simpleVampire4_1)
    g.verifyProofStep(dropFirstColRawPreservesWelltypedRawstepcasePS, simpleVampire4_1)
  }


}

object SQLSoundnessProofGraph {
  def initializeGraphTypes(g: ProofGraphXodus[VeritasConstruct, VeritasConstruct]) = {
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
    PropertyTypes.registerPropertyType[Solve[_, _]](g.store)
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
  }
}


// Executing this object creates a new SQL Soundness Proof Graph,
// attempting to verify as much as possible
object ConstructSQLSoundnessGraph extends App {

  val file = new File("SQLSoundnessProofGraph-store")
  file.delete() //simply overwrite any old folder
  //try to create new folder
  if (!file.mkdir()) sys.error("Could not create new store for SQLSoundnessProofGraph-store.")

  val pg = new SQLSoundnessProofGraph(file)

  pg.verifySingleStepsSimple()

}

