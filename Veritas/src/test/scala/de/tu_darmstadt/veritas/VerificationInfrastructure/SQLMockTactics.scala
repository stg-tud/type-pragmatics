package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics._
import de.tu_darmstadt.veritas.backend.ast._

object SQLMockTactics {
  //This object contains all MockTactics and hand-coded obligations for the SQL soundness proof graph

  import SQLSoundnessProofSteps._

  //Mock tactics
  // class for creating mock induction tactics, with convenience methods like selectCase
  class MockInduction(inductionvar: VeritasConstruct) extends Tactic[VeritasConstruct, VeritasFormula] {

    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]): Iterable[(Obligation, EdgeLabel)] =
      Seq()
  }

  object MockInduction {
    def selectCase[Obligation](name: String, required: Iterable[(Obligation, EdgeLabel)]): Obligation =
      required.find(_._2.asInstanceOf[StructInductCase[VeritasConstruct, VeritasFormula]].casename == name).get._1

    def selectCase[ProofStep, Spec, Goal](g: ProofGraph[Spec, Goal])(name: String, ps: g.ProofStep): g.Obligation =
      g.requiredObls(ps).find(_._2.asInstanceOf[StructInductCase[VeritasConstruct, VeritasFormula]].casename == name).get._1
  }

  // Apply structural induction to progress root via ad-hoc instance of MockInduction,
  // where the goals that are supposed to be generated are just hard-coded


  object rootInductionProgress extends MockInduction(MetaVar("q")) {

    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val tvaluecase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTtvalue)

      val selectfromwherecase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTselectFromWhere)

      val unioncase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTUnion)

      val intersectioncase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTIntersection)

      val differencecase: Obligation = produce.newObligation(fullSQLspec, SQLProgressTDifference)


      Seq((tvaluecase, StructInductCase[VeritasConstruct, VeritasFormula](SQLProgressTtvalue.goals.head.name,
        Seq(), Seq(), Seq())),
        (selectfromwherecase, StructInductCase[VeritasConstruct, VeritasFormula](SQLProgressTselectFromWhere.goals.head.name,
          Seq(), Seq(), Seq())),
        (unioncase, StructInductCase[VeritasConstruct, VeritasFormula](SQLProgressTUnion.goals.head.name,
          unionconsts map ((c: VeritasConstruct) => FixedVar(c)), //TODO: think about a good way to get rid of the upcast
          Seq(InductionHypothesis[VeritasFormula](Axioms(Seq(SQLProgressTUnionIH1.axioms.head))), InductionHypothesis[VeritasFormula](Axioms(Seq(SQLProgressTUnionIH2.axioms.head)))), Seq())),
        (intersectioncase, StructInductCase[VeritasConstruct, VeritasFormula](SQLProgressTIntersection.goals.head.name,
          intersectionconsts map ((c: VeritasConstruct) => FixedVar(c)), //TODO: think about a good way to get rid of the upcast
          Seq(InductionHypothesis[VeritasFormula](Axioms(Seq(SQLProgressTIntersectionIH1.axioms.head))), InductionHypothesis[VeritasFormula](Axioms(Seq(SQLProgressTIntersectionIH2.axioms.head)))), Seq())),
        (differencecase, StructInductCase[VeritasConstruct, VeritasFormula](SQLProgressTDifference.goals.head.name,
          differenceconsts map ((c: VeritasConstruct) => FixedVar(c)), //TODO: think about a good way to get rid of the upcast
          Seq(InductionHypothesis[VeritasFormula](Axioms(Seq(SQLProgressTDifferenceIH1.axioms.head))), InductionHypothesis[VeritasFormula](Axioms(Seq(SQLProgressTDifferenceIH2.axioms.head)))), Seq())))

    }
  }

  object successfulLookupInduction extends MockInduction(MetaVar("TS")) {

    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val successfulLookupEmptyObl = produce.newObligation(fullSQLspec, successfulLookupEmpty)
      val successfulLookupBindObl = produce.newObligation(fullSQLspec, successfulLookupBind)

      Seq((successfulLookupEmptyObl, StructInductCase[VeritasConstruct, VeritasFormula](successfulLookupEmpty.goals.head.name,
        Seq(), Seq(), Seq())),
        (successfulLookupBindObl, StructInductCase[VeritasConstruct, VeritasFormula](successfulLookupBind.goals.head.name,
          Seq(FixedVar(successfulLookupBindConsts)),
          Seq(InductionHypothesis[VeritasFormula](successfulLookupBindIH)), Seq())))

    }
  }


  object welltypedLookupInduction extends MockInduction(MetaVar("TS")) {

    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val welltypedLookupEmptyObl = produce.newObligation(fullSQLspec, welltypedLookupEmpty)
      val welltypedLookupBindObl = produce.newObligation(fullSQLspec, welltypedLookupBind)

      Seq((welltypedLookupEmptyObl, StructInductCase[VeritasConstruct, VeritasFormula](welltypedLookupEmpty.goals.head.name,
        Seq(), Seq(), Seq())),
        (welltypedLookupBindObl, StructInductCase[VeritasConstruct, VeritasFormula](welltypedLookupBind.goals.head.name,
          Seq(FixedVar(welltypedLookupConsts)),
          Seq(InductionHypothesis[VeritasFormula](welltypedLookupBindIH)), Seq())))

    }


  }

  object filterRowsPreservesTableInduction extends MockInduction(MetaVar("rt")) {
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val filterRowsPreservesTableTemptyObl = produce.newObligation(fullSQLspec, filterRowsPreservesTableTempty)
      val filterRowsPreservesTableTconsObl = produce.newObligation(fullSQLspec, filterRowsPreservesTableTcons)

      Seq((filterRowsPreservesTableTemptyObl, StructInductCase[VeritasConstruct, VeritasFormula](filterRowsPreservesTableTempty.goals.head.name,
        Seq(), Seq(), Seq())),
        (filterRowsPreservesTableTconsObl, StructInductCase[VeritasConstruct, VeritasFormula](filterRowsPreservesTableTcons.goals.head.name,
          Seq(FixedVar(filterRowsPreservesTableTconsConsts)),
          Seq(InductionHypothesis[VeritasFormula](filterRowsPreservesTableTconsIH)), Seq())))

    }
  }


  object projectColsProgressInduction extends MockInduction(MetaVar("al2")) {
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val projectColsProgressAemptyObl = produce.newObligation(fullSQLspec, projectColsProgressAempty)
      val projectColsProgressAconsObl = produce.newObligation(fullSQLspec, projectColsProgressAcons)

      Seq((projectColsProgressAemptyObl, StructInductCase[VeritasConstruct, VeritasFormula](projectColsProgressAempty.goals.head.name,
        Seq(), Seq(), Seq())),
        (projectColsProgressAconsObl, StructInductCase[VeritasConstruct, VeritasFormula](projectColsProgressAcons.goals.head.name,
          Seq(FixedVar(projectColsProgressAconsConsts)),
          Seq(InductionHypothesis[VeritasFormula](projectColsProgressAconsIH)), Seq())))

    }
  }

  object projectTypeImpliesFindColInduction extends MockInduction(MetaVar("al2")) {
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val projectTypeImpliesFindColAemptyObl = produce.newObligation(fullSQLspec, projectTypeImpliesFindColAempty)
      val projectTypeImpliesFindColAconsObl = produce.newObligation(fullSQLspec, projectTypeImpliesFindColAcons)

      Seq((projectTypeImpliesFindColAemptyObl, StructInductCase[VeritasConstruct, VeritasFormula](projectTypeImpliesFindColAempty.goals.head.name,
        Seq(), Seq(), Seq())),
        (projectTypeImpliesFindColAconsObl, StructInductCase[VeritasConstruct, VeritasFormula](projectTypeImpliesFindColAcons.goals.head.name,
          Seq(FixedVar(projectTypeImpliesFindColAconsConsts)),
          Seq(InductionHypothesis[VeritasFormula](projectTypeImpliesFindColAconsIH)), Seq())))

    }
  }

  object findColTypeImpliesfindColInduction extends MockInduction(MetaVar("al")) {
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val findColTypeImpliesfindColAemptyObl = produce.newObligation(fullSQLspec, findColTypeImpliesfindColAempty)
      val findColTypeImpliesfindColAconsObl = produce.newObligation(fullSQLspec, findColTypeImpliesfindColAcons)

      Seq((findColTypeImpliesfindColAemptyObl, StructInductCase[VeritasConstruct, VeritasFormula](findColTypeImpliesfindColAempty.goals.head.name,
        Seq(), Seq(), Seq())),
        (findColTypeImpliesfindColAconsObl, StructInductCase[VeritasConstruct, VeritasFormula](findColTypeImpliesfindColAcons.goals.head.name,
          Seq(FixedVar(findColTypeImpliesfindColAconsConsts)),
          Seq(InductionHypothesis[VeritasFormula](findColTypeImpliesfindColAconsIH)), Seq())))

    }
  }

  object projectTypeAttrLImpliesfindAllColTypeInduction extends MockInduction(MetaVar("al")) {
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val projectTypeAttrLImpliesfindAllColTypeAemptyObl = produce.newObligation(fullSQLspec, projectTypeAttrLImpliesfindAllColTypeAempty)
      val projectTypeAttrLImpliesfindAllColTypeAconsObl = produce.newObligation(fullSQLspec, projectTypeAttrLImpliesfindAllColTypeAcons)

      Seq((projectTypeAttrLImpliesfindAllColTypeAemptyObl, StructInductCase[VeritasConstruct, VeritasFormula](projectTypeAttrLImpliesfindAllColTypeAempty.goals.head.name,
        Seq(), Seq(), Seq())),
        (projectTypeAttrLImpliesfindAllColTypeAconsObl, StructInductCase[VeritasConstruct, VeritasFormula](projectTypeAttrLImpliesfindAllColTypeAcons.goals.head.name,
          Seq(FixedVar(projectTypeAttrLImpliesfindAllColTypeAconsConsts)),
          Seq(InductionHypothesis[VeritasFormula](projectTypeAttrLImpliesfindAllColTypeAconsIH)), Seq())))

    }
  }

  object dropFirstColRawPreservesWelltypedRawInduction extends MockInduction(MetaVar("rt")) {
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
      val dropFirstColRawPreservesWelltypedRawTemptyObl = produce.newObligation(fullSQLspec, dropFirstColRawPreservesWelltypedRawTempty)
      val dropFirstColRawPreservesWelltypedRawTconsObl = produce.newObligation(fullSQLspec, dropFirstColRawPreservesWelltypedRawTcons)

      Seq((dropFirstColRawPreservesWelltypedRawTemptyObl, StructInductCase[VeritasConstruct, VeritasFormula](dropFirstColRawPreservesWelltypedRawTempty.goals.head.name,
        Seq(), Seq(), Seq())),
        (dropFirstColRawPreservesWelltypedRawTconsObl, StructInductCase[VeritasConstruct, VeritasFormula](dropFirstColRawPreservesWelltypedRawTcons.goals.head.name,
          Seq(FixedVar(dropFirstColRawPreservesWelltypedRawTconsConsts)),
          Seq(InductionHypothesis[VeritasFormula](dropFirstColRawPreservesWelltypedRawTconsIH)), Seq())))

    }
  }


  //class for creating mock case distinctions
  class MockCaseDistinction(cases: Seq[VeritasConstruct]) extends Tactic[VeritasConstruct, VeritasFormula] {

    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]): Iterable[(Obligation, EdgeLabel)] =
      Seq()
  }

  object MockCaseDistinction {
    def selectCase[Obligation](name: String, required: Iterable[(Obligation, EdgeLabel)]): Obligation =
      required.find(_._2.asInstanceOf[CaseDistinctionCase[VeritasConstruct, VeritasFormula]].casename == name).get._1
  }


  // hard coded tactic for case distinction of union/intersection/difference induction case
  case class SetCaseDistinction(setsym: Symbol, setname: String)
    extends MockCaseDistinction(Seq(case1pred, case2pred, case3pred) map
      ((stj: Seq[TypingRuleJudgment]) => Goals(Seq(TypingRule("casepreds", Seq(), stj)), None))) {

    val casepreds = Seq(case1pred, case2pred, case3pred)

    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]):
    Iterable[(Obligation, EdgeLabel)] = {
      def mkcase(i: Int): Obligation = produce.newObligation(fullSQLspec,
        mkSQLProgressTSetCase(i, setsym, setname, casepreds(i)))

      //note: a real tactic would have to extract the information to be propagated
      // in the edges from the given obllabels (unused here) and decide which one
      // to forward where
      Seq((mkcase(0), CaseDistinctionCase[VeritasConstruct, VeritasFormula](setname + "1", Seq(), Seq(),
        (setconsts map ((sc: VeritasConstruct) => FixedVar(sc))))),
        (mkcase(1), CaseDistinctionCase[VeritasConstruct, VeritasFormula](setname + "2", Seq(), Seq(),
          (setconsts map ((sc: VeritasConstruct) => FixedVar(sc))) :+
            InductionHypothesis[VeritasFormula](mkSQLProgressTSetCaseIH(2, setname, 'q2)))),
        (mkcase(2), CaseDistinctionCase[VeritasConstruct, VeritasFormula](setname + "3", Seq(), Seq(),
          (setconsts map ((sc: VeritasConstruct) => FixedVar(sc))) :+
            InductionHypothesis[VeritasFormula](mkSQLProgressTSetCaseIH(1, setname, 'q1)))))
    }

  }

  //class for creating mock lemma generation tactics
  case class MockLemmaApplication(lemmas: Seq[Lemmas]) extends Tactic[VeritasConstruct, VeritasFormula] {
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
    override def apply[Obligation](obl: GenObligation[VeritasConstruct, VeritasFormula],
                                   obllabels: Iterable[EdgeLabel],
                                   produce: ObligationProducer[VeritasConstruct, VeritasFormula, Obligation]): Iterable[(Obligation, EdgeLabel)] =
      for (lem <- lemmas) yield
        produce.newObligation(fullSQLspec, Goals(lem.lemmas, lem.timeout)) -> LemmaApplicationStep(lem.lemmas.head.name)

  }

  object MockLemmaApplication {
    def selectLemma[Obligation](name: String, required: Iterable[(Obligation, EdgeLabel)]): Obligation =
      required.find(_._2.asInstanceOf[LemmaApplicationStep[VeritasFormula]].lemmaname == name).get._1
  }


}
