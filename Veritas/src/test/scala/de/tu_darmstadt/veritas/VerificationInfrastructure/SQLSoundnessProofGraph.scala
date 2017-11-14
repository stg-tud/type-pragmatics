package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.ConstructSQLSoundnessGraph.pg
import de.tu_darmstadt.veritas.VerificationInfrastructure.SQLMockTactics._
import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.VeritasSpecEnquirer
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics._
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{Finished, TPTPVampireVerifier, TSTPProof, VerifierFailure}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpApp, FunctionMeta}
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}
import org.scalatest.FunSuite

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
      Seq((mkcase(0), CaseDistinctionCase[VeritasConstruct, VeritasFormula](setname + "1",
        (setconsts map ((sc: VeritasConstruct) => FixedVar(sc))))),
        (mkcase(1), CaseDistinctionCase[VeritasConstruct, VeritasFormula](setname + "2",
          (setconsts map ((sc: VeritasConstruct) => FixedVar(sc))) :+
            InductionHypothesis[VeritasFormula](mkSQLProgressTSetCaseIH(2, setname, 'q2)))),
        (mkcase(2), CaseDistinctionCase[VeritasConstruct, VeritasFormula](setname + "3",
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


// Constructing the SQL soundness proof graph
// We instantiate S = VeritasConstruct (should be a Module) and P = VeritasConstruct (Should be Goal/local)
class SQLSoundnessProofGraph(file: File) {

  import SQLMockTactics._
  import SQLSoundnessProofSteps._

  val g: ProofGraphXodus[VeritasConstruct, VeritasFormula] with ProofGraphTraversals[VeritasConstruct, VeritasFormula] =
    new ProofGraphXodus[VeritasConstruct, VeritasFormula](file) with ProofGraphTraversals[VeritasConstruct, VeritasFormula]
  SQLSoundnessProofGraph.initializeGraphTypes(g)

  val specenq = new VeritasSpecEnquirer(fullSQLspec)


  //add progress root obligation
  val progressObligation: g.Obligation = g.newObligation(fullSQLspec, SQLProgress)
  g.storeObligation("SQL progress", progressObligation)

  //apply structural induction on a given induction var to a given obligation and retrieve all resulting obligations
  def applyInductionGetCases(obl: g.Obligation, indvar: MetaVar): Map[String, (g.Obligation, EdgeLabel)] = {
    val indtac = StructuralInduction(indvar, fullSQLspec, specenq)
    val ps = g.applyTactic(obl, indtac)
    val subobls = g.requiredObls(ps)
    indtac.enumerateCases(subobls)
  }

  // mutual function: apply Solve tactic to all sub-obligations of a given obligation and return the resulting proof steps
  def applySolveToAllSub(obl: g.Obligation): Seq[g.ProofStep] = {
    g.appliedStep(obl) match {
      case Some(ps) => {
        val subobls = g.requiredObls(ps).map(_._1)
        (for (o <- subobls) yield g.applyTactic(o, Solve[VeritasConstruct, VeritasFormula])).toSeq
      }
      case None => Seq()
    }

  }

  //private val rootInduction = StructuralInduction(MetaVar("q"), fullSQLspec, specenq)
  // first proof step: structural induction
  //val rootinductionPS: g.ProofStep = g.applyTactic(progressObligation, rootInductionProgress) //mock tactic with hardcoded steps
  //val rootinductionPS: g.ProofStep = g.applyTactic(progressObligation, rootInduction)

  val rootobl = g.findObligation("SQL progress").get
  val rootobl_edge_map = applyInductionGetCases(rootobl, MetaVar("q"))

  //val rootsubobs = g.requiredObls(rootinductionPS)
  val rootcasenames = rootobl_edge_map.keys.toSeq.sortWith(_ < _)
  //val caseedges: Seq[StructInductCase[VeritasConstruct, VeritasFormula]] =
  //  (rootInduction.enumerateCases(rootsubobs) map
  //    {case (k, v) => v._2.asInstanceOf[StructInductCase[VeritasConstruct, VeritasFormula]]}).toSeq

  //apply simply Solve-tactic to t-value base case
  val tvaluecaseobl = rootobl_edge_map(rootcasenames(0))._1
  val tvaluecasePS = g.applyTactic(tvaluecaseobl, Solve[VeritasConstruct, VeritasFormula])

  // Case distinctions for Union, Intersection, Difference cases
  // construct the case predicates:

  def makeSetCasePreds(fv1name: String, fv2name: String): Seq[Seq[TypingRuleJudgment]] = {
    import FunctionDSL._
    import SymTreeDSL._
    import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._

    //TODO: here t1 and t2 are guessed to be free variable names (which will work out in this particular case) - better: generate fresh names
    val case1pred: Seq[TypingRuleJudgment] = (~(Symbol(fv1name)) === 'tvalue (~'t1)) & (~(Symbol(fv2name)) === 'tvalue (~'t2))
    val case2pred: Seq[TypingRuleJudgment] = (~(Symbol(fv1name)) === 'tvalue (~'t1)) & (forall(~'t2) | (~(Symbol(fv2name)) ~= 'tvalue (~'t2)))
    val case3pred: Seq[TypingRuleJudgment] = Seq(forall(~'t1) | (~(Symbol(fv1name)) ~= 'tvalue (~'t1)))

    Seq(case1pred, case2pred, case3pred)
  }

  def makeSetCaseDistinction(fv1name: String, fv2name: String): CaseDistinction[VeritasConstruct, VeritasFormula] =
    CaseDistinction(makeSetCasePreds(fv1name, fv2name), fullSQLspec, specenq)

  def extract2FVNames(casename: String): (String, String) = {
    val cases = rootobl_edge_map(casename)._2
    val (fv1, fv2) = cases match {
      case StructInductCase(_, fv, _, _) => (fv(0).fixedvar, fv(1).fixedvar)
      case _ => sys.error(s"Expected a StructInductCase label at the given casename $casename")
    }

    (fv1, fv2) match {
      case (MetaVar(n1), MetaVar(n2)) => (n1, n2)
      case (FunctionMeta(MetaVar(n1)), FunctionMeta(MetaVar(n2))) => (n1, n2)
      case _ => sys.error("fixed variables did not have expected format")
    }
  }


  //val unionCaseDistinction = SetCaseDistinction(unionsym, sunion) //hard-coded mock tactic
  val unioncaseFVs = extract2FVNames(rootcasenames(2))
  val unionCaseDistinction = makeSetCaseDistinction(unioncaseFVs._1, unioncaseFVs._2)

  val unioncaseobl = rootobl_edge_map(rootcasenames(2))._1
  val unioncasePS = g.applyTactic(unioncaseobl, unionCaseDistinction)

  //val intersectionCaseDistinction = SetCaseDistinction(intersectionsym, sintersection) //hard-coded mock tactic
  val intersectioncaseFVs = extract2FVNames(rootcasenames(3))
  val intersectionCaseDistinction = makeSetCaseDistinction(intersectioncaseFVs._1, intersectioncaseFVs._2)

  val intersectioncaseobl = rootobl_edge_map(rootcasenames(3))._1
  val intersectioncasePS = g.applyTactic(intersectioncaseobl, intersectionCaseDistinction)

  //val differenceCaseDistinction = SetCaseDistinction(differencesym, sdifference) //hard-coded mock tactic
  val differencecaseFVs = extract2FVNames(rootcasenames(4))
  val differenceCaseDistinction = makeSetCaseDistinction(differencecaseFVs._1, differencecaseFVs._2)

  val differencecaseobl = rootobl_edge_map(rootcasenames(4))._1
  val differencecasePS = g.applyTactic(differencecaseobl, differenceCaseDistinction)

  //apply Solve tactic to all of the set cases
  val setPS = (for (c <- Seq(unioncaseobl, intersectioncaseobl, differencecaseobl)) yield {
    applySolveToAllSub(c)
  }).flatten


  //prove selectFromWhereCase via auxiliary lemmas:
  val selcase = rootobl_edge_map(rootcasenames(1))._1


  //apply lemma application tactic to selection case
  val selLemmaTac = MockLemmaApplication(Seq(successfulLookup, welltypedLookup,
    filterPreservesType, projectTableProgress))
  val selLemmaPS = g.applyTactic(selcase, selLemmaTac)


  // prove lemma successfulLookup via simple structural induction, cases via Solve
  val successfulLookupobl = MockLemmaApplication.selectLemma(successfulLookup.lemmas.head.name,
    g.requiredObls(selLemmaPS))
  applyInductionGetCases(successfulLookupobl, MetaVar("TS"))
  val successfulLookup_cases_PS = applySolveToAllSub(successfulLookupobl)


  // prove lemma welltypedLookup via simple structural induction
  val welltypedLookupobl = MockLemmaApplication.selectLemma(welltypedLookup.lemmas.head.name,
    g.requiredObls(selLemmaPS))
  applyInductionGetCases(welltypedLookupobl, MetaVar("TS"))
  val welltypedLookup_cases_PS = applySolveToAllSub(welltypedLookupobl)


  // prove lemma filterPreservesType via auxiliary lemma filterRowsPreservesTable
  val filterPreservesTypeobl = MockLemmaApplication.selectLemma(filterPreservesType.lemmas.head.name,
    g.requiredObls(selLemmaPS))

  val filterPreservesTypePS = g.applyTactic(filterPreservesTypeobl,
    MockLemmaApplication(Seq(filterRowsPreservesTable)))

  val filterRowsPreservesTableObl = MockLemmaApplication.selectLemma(filterRowsPreservesTable.lemmas.head.name,
    g.requiredObls(filterPreservesTypePS))

  // prove lemmma filterRowsPreservesTable via simple structural induction
  applyInductionGetCases(filterRowsPreservesTableObl, MetaVar("rt"))
  val filterRowsPreservesTablePS = applySolveToAllSub(filterRowsPreservesTableObl)

  //try to prove projectTableProgress via lemma application with projectColsProgress?
  //yes, works, apparently no case distinction necessary!
  val projectTableProgressobl = MockLemmaApplication.selectLemma(projectTableProgress.lemmas.head.name,
    g.requiredObls(selLemmaPS))

  val projectTableProgressPS = g.applyTactic(projectTableProgressobl,
    MockLemmaApplication(Seq(projectColsProgress)))

  //prove projectColsProgress via induction
  val projectColsProgressObl = MockLemmaApplication.selectLemma(projectColsProgress.lemmas.head.name,
    g.requiredObls(projectTableProgressPS))

  val projectColsProgress_casemap = applyInductionGetCases(projectColsProgressObl, MetaVar("al2"))
  val projectColsProgress_casenames = projectColsProgress_casemap.keys.toSeq.sortWith(_ < _)

  val projectColsProgressbasecase = projectColsProgress_casemap(projectColsProgress_casenames(0))._1

  val projectColsProgressbasecasePS = g.applyTactic(projectColsProgressbasecase,
    Solve[VeritasConstruct, VeritasFormula])

  // step case requires an auxiliary lemma (projectTypeImpliesFindCol)
  val projectColsProgressstepcase = projectColsProgress_casemap(projectColsProgress_casenames(1))._1
  val projectColsProgressstepcasePS = g.applyTactic(projectColsProgressstepcase,
    MockLemmaApplication(Seq(projectTypeImpliesFindCol)))

  //prove projectTypeImpliesFindCol via induction
  val projectTypeImpliesFindColObl = MockLemmaApplication.selectLemma(projectTypeImpliesFindCol.lemmas.head.name,
    g.requiredObls(projectColsProgressstepcasePS))

  val projectTypeImpliesFindCol_casemap = applyInductionGetCases(projectTypeImpliesFindColObl, MetaVar("al2"))
  val projectTypeImpliesFindCol_casenames = projectTypeImpliesFindCol_casemap.keys.toSeq.sortWith(_ < _)

  val projectTypeImpliesFindColbasecase = projectTypeImpliesFindCol_casemap(projectTypeImpliesFindCol_casenames(0))._1
  val projectTypeImpliesFindColbasecasePS = g.applyTactic(projectTypeImpliesFindColbasecase,
    Solve[VeritasConstruct, VeritasFormula])

  // step case requires two auxiliary lemmas
  val projectTypeImpliesFindColstepcase = projectTypeImpliesFindCol_casemap(projectTypeImpliesFindCol_casenames(1))._1
  val projectTypeImpliesFindColstepcasePS = g.applyTactic(projectTypeImpliesFindColstepcase,
    MockLemmaApplication(Seq(findColTypeImpliesfindCol, projectTypeAttrLImpliesfindAllColType)))

  //prove findColTypeImpliesfindCol via induction
  val findColTypeImpliesfindColObl = MockLemmaApplication.selectLemma(findColTypeImpliesfindCol.lemmas.head.name,
    g.requiredObls(projectTypeImpliesFindColstepcasePS))

  val findColTypeImpliesfindCol_casemap = applyInductionGetCases(findColTypeImpliesfindColObl, MetaVar("al"))
  val findColTypeImpliesfindCol_casenames = findColTypeImpliesfindCol_casemap.keys.toSeq.sortWith(_ < _)

  val findColTypeImpliesfindColbasecase = findColTypeImpliesfindCol_casemap(findColTypeImpliesfindCol_casenames(0))._1
  val findColTypeImpliesfindColbasecasePS = g.applyTactic(findColTypeImpliesfindColbasecase,
    Solve[VeritasConstruct, VeritasFormula])

  //step requires auxiliary lemma dropFirstColRawPreservesWelltypedRaw
  val findColTypeImpliesfindColstepcase = findColTypeImpliesfindCol_casemap(findColTypeImpliesfindCol_casenames(1))._1
  val findColTypeImpliesfindColstepcasePS = g.applyTactic(findColTypeImpliesfindColstepcase,
    MockLemmaApplication(Seq(dropFirstColRawPreservesWelltypedRaw)))

  //prove projectTypeAttrLImpliesfindAllColType via induction
  val projectTypeAttrLImpliesfindAllColTypeObl = MockLemmaApplication.selectLemma(projectTypeAttrLImpliesfindAllColType.lemmas.head.name,
    g.requiredObls(projectTypeImpliesFindColstepcasePS))

  applyInductionGetCases(projectTypeAttrLImpliesfindAllColTypeObl, MetaVar("al"))
  val projectTypeAttrLImpliesfindAllColTypePS = applySolveToAllSub(projectTypeAttrLImpliesfindAllColTypeObl)

  //prove dropFirstColRawPreservesWelltypedRaw via induction
  val dropFirstColRawPreservesWelltypedRawObl = MockLemmaApplication.selectLemma(dropFirstColRawPreservesWelltypedRaw.lemmas.head.name,
    g.requiredObls(findColTypeImpliesfindColstepcasePS))

  applyInductionGetCases(dropFirstColRawPreservesWelltypedRawObl, MetaVar("rt"))
  val dropFirstColRawPreservesWelltypedRawPS = applySolveToAllSub(dropFirstColRawPreservesWelltypedRawObl)

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
    for (ps <- successfulLookup_cases_PS)
      g.verifyProofStep(ps, simpleVampire4_1)

    for (ps <- welltypedLookup_cases_PS)
      g.verifyProofStep(ps, simpleVampire4_1)


    g.verifyProofStep(filterPreservesTypePS, simpleVampire4_1)

    for (ps <- filterRowsPreservesTablePS)
        g.verifyProofStep(ps, simpleVampire4_1)

    g.verifyProofStep(projectTableProgressPS, simpleVampire4_1_20)

    g.verifyProofStep(projectColsProgressbasecasePS, simpleVampire4_1)
    g.verifyProofStep(projectColsProgressstepcasePS, simpleVampire4_1)

    g.verifyProofStep(projectTypeImpliesFindColbasecasePS, simpleVampire4_1)
    g.verifyProofStep(projectTypeImpliesFindColstepcasePS, simpleVampire4_1)

    g.verifyProofStep(findColTypeImpliesfindColbasecasePS, simpleVampire4_1)
    g.verifyProofStep(findColTypeImpliesfindColstepcasePS, simpleVampire4_1)

    for (ps <- projectTypeAttrLImpliesfindAllColTypePS)
      g.verifyProofStep(ps, simpleVampire4_1)

    for (ps <- dropFirstColRawPreservesWelltypedRawPS)
      g.verifyProofStep(ps, simpleVampire4_1)

  }


}

object SQLSoundnessProofGraph {
  def initializeGraphTypes(g: ProofGraphXodus[VeritasConstruct, VeritasFormula]) = {
    PropertyTypes.registerWrapperType(g.store)
    //register all the necessary property types
    //PropertyTypes.registerPropertyType[VeritasConstruct](g.store)
    //PropertyTypes.registerPropertyType[VeritasFormula](g.store)
    //PropertyTypes.registerPropertyType[VeritasFormula with Ordered[VeritasFormula]](g.store)
    //PropertyTypes.registerPropertyType[Module](g.store)
    //PropertyTypes.registerPropertyType[Goals](g.store)
    //PropertyTypes.registerPropertyType[rootInductionProgress.type](g.store)
    //PropertyTypes.registerPropertyType[StructInductCase[VeritasConstruct, VeritasFormula]](g.store)
    //PropertyTypes.registerPropertyType[SetCaseDistinction](g.store)
    //PropertyTypes.registerPropertyType[CaseDistinctionCase[VeritasConstruct, VeritasFormula]](g.store)
    //PropertyTypes.registerPropertyType[Finished[_, _]](g.store)
    //PropertyTypes.registerPropertyType[VerifierFailure[_, _]](g.store)
    //PropertyTypes.registerPropertyType[TSTPProof](g.store)
    //PropertyTypes.registerPropertyType[Solve[_, _]](g.store)
    //PropertyTypes.registerPropertyType[MockLemmaApplication](g.store)
    //PropertyTypes.registerPropertyType[LemmaApplicationStep[_]](g.store)
    //PropertyTypes.registerPropertyType[successfulLookupInduction.type](g.store)
    //PropertyTypes.registerPropertyType[welltypedLookupInduction.type](g.store)
    //PropertyTypes.registerPropertyType[filterRowsPreservesTableInduction.type](g.store)
    //PropertyTypes.registerPropertyType[projectColsProgressInduction.type](g.store)
    //PropertyTypes.registerPropertyType[projectTypeImpliesFindColInduction.type](g.store)
    //PropertyTypes.registerPropertyType[findColTypeImpliesfindColInduction.type](g.store)
    //PropertyTypes.registerPropertyType[projectTypeAttrLImpliesfindAllColTypeInduction.type](g.store)
    //PropertyTypes.registerPropertyType[dropFirstColRawPreservesWelltypedRawInduction.type](g.store)
    //PropertyTypes.registerPropertyType[verifier.Unknown[_, _]](g.store)
  }
}


// Executing this object creates a new SQL Soundness Proof Graph,
// attempting to verify as much as possible
object ConstructSQLSoundnessGraph extends App {

  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }

  val file = new File("SQLSoundnessProofGraph-store")
  recursivedelete(file) //simply overwrite any old folder
  //try to create new folder
  if (!file.mkdir()) sys.error("Could not create new store for SQLSoundnessProofGraph-store.")

  val pg = new SQLSoundnessProofGraph(file)


}

