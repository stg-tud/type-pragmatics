package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.{CaseDistinctionCase, Solve}
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{Unknown, VerifierFailure}
import de.tu_darmstadt.veritas.backend.ast.VeritasConstruct
import org.scalatest.FunSuite


/**
  * Constructing the SQL soundness proof graph and tests on the graph
  */
class SQLSoundnessProofGraphTest extends FunSuite {

  import SQLSoundnessProofSteps._
  import de.tu_darmstadt.veritas.VerificationInfrastructure.SQLMockTactics._

  //Tries to load existing SQLSoundnessGraph
  val file = new File("SQLSoundnessProofGraph-store")
  if (!file.isDirectory) {
    sys.error("Test store SQLSoundnessProofGraph-store does not exist.")
  }

  val loaded_g: ProofGraphXodus[VeritasConstruct, VeritasConstruct] with ProofGraphTraversals[VeritasConstruct, VeritasConstruct] =
    new ProofGraphXodus[VeritasConstruct, VeritasConstruct](file) with ProofGraphTraversals[VeritasConstruct, VeritasConstruct]

  SQLSoundnessProofGraph.initializeGraphTypes(loaded_g)

  val progressobl = loaded_g.findObligation("SQL Progress")

  test("Root obligation (progress) can be successfully retrieved from loaded graph") {
    assert(loaded_g.storedObligations.size == 1)
    assert(progressobl.nonEmpty)
    assert(progressobl.get.spec == fullSQLspec)
    assert(progressobl.get.goal == SQLProgress)
  }

  val progressindPS = loaded_g.appliedStep(progressobl.get)
  val obls = loaded_g.requiredObls(progressindPS.get)
  val tvaluecase = MockInduction.selectCase(SQLProgressTtvalue.goals.head.name, obls)
  val selcase = MockInduction.selectCase(SQLProgressTselectFromWhere.goals.head.name, obls)
  val unioncase = MockInduction.selectCase(SQLProgressTUnion.goals.head.name, obls)
  val intersectioncase = MockInduction.selectCase(SQLProgressTIntersection.goals.head.name, obls)
  val differencecase = MockInduction.selectCase(SQLProgressTDifference.goals.head.name, obls)

  test("All root induction cases are retrievable") {
    assert(progressindPS.nonEmpty)

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


  test("All progress induction cases have proof steps") {
    assert(loaded_g.appliedStep(tvaluecase).nonEmpty)
    assert(loaded_g.appliedStep(selcase).nonEmpty)
    assert(loaded_g.appliedStep(unioncase).nonEmpty)
    assert(loaded_g.appliedStep(intersectioncase).nonEmpty)
    assert(loaded_g.appliedStep(differencecase).nonEmpty)

  }

  val tvaluecasePS = loaded_g.appliedStep(tvaluecase).get
  val selcasePS = loaded_g.appliedStep(selcase).get
  val unioncasePS = loaded_g.appliedStep(unioncase).get
  val intersectioncasePS = loaded_g.appliedStep(intersectioncase).get
  val differencecasePS = loaded_g.appliedStep(differencecase).get

  test("T-value base case was verified") {
    assert(loaded_g.verifiedBy(tvaluecasePS).nonEmpty)

    val tvalueres = loaded_g.verifiedBy(tvaluecasePS).get

    //TODO: isStepVerified and isOblVerified both call EvidenceChecker - is that a good idea?
    assert(tvalueres.status.isVerified)
    assert(tvalueres.errorMsg.isEmpty)
    assert(tvalueres.evidence.nonEmpty)
    assert(tvaluecasePS.tactic.allRequiredOblsVerified(loaded_g)(tvaluecase, loaded_g.requiredObls(tvaluecasePS)))
  }

  val unionobls = loaded_g.requiredObls(unioncasePS)
  val intersectionobls = loaded_g.requiredObls(intersectioncasePS)
  val differenceobls = loaded_g.requiredObls(differencecasePS)

  test("Applying case distinctions worked as desired") {

    val union1 = MockCaseDistinction.selectCase("Union1", unionobls)
    val intersection3 = MockCaseDistinction.selectCase("Intersection3", intersectionobls)
    val difference2 = MockCaseDistinction.selectCase("Difference2", differenceobls)

    assert(union1.goal == mkSQLProgressTSetCase(0, unionsym, sunion, case1pred))
    assert(intersection3.goal == mkSQLProgressTSetCase(2, intersectionsym, sintersection, case3pred))
    assert(difference2.goal == mkSQLProgressTSetCase(1, differencesym, sdifference, case2pred))

    //compare some induction hypotheses
    val obls = loaded_g.requiredObls(progressindPS.get)
    val unioncase = MockInduction.selectCase(SQLProgressTUnion.goals.head.name, obls)
    val intersectioncase = MockInduction.selectCase(SQLProgressTIntersection.goals.head.name, obls)
    val differencecase = MockInduction.selectCase(SQLProgressTDifference.goals.head.name, obls)

    val unionps = loaded_g.appliedStep(unioncase).get
    val unionedges = loaded_g.requiredObls(unionps).toSeq
    assert(unionedges.size == 3)
    assert(unionedges(1)._2.asInstanceOf[CaseDistinctionCase[VeritasConstruct]].ihs.ihs == SQLProgressTUnionIH2)

  }

  test("Not proven individual set cases (tvalue+tvalue) in loaded graph") {
    val successfulObls = List(loaded_g.requiredObls(unioncasePS).head,
      loaded_g.requiredObls(intersectioncasePS).head, loaded_g.requiredObls(differencecasePS).head)

    for ((o, e) <- successfulObls) {
      val stepO = loaded_g.appliedStep(o)
      assert(stepO.nonEmpty)
      val step = stepO.get
      val resO = loaded_g.verifiedBy(step)
      assert(resO.nonEmpty)
      val res = resO.get

      assert(!res.status.isVerified)
      assert(res.errorMsg.nonEmpty)
      assert(res.evidence.isEmpty)
    }
  }


  test("Timeout for remaining individual set cases (SQL progress proof) in loaded graph") {
    val unsuccessfulObls = loaded_g.requiredObls(unioncasePS).tail ++
      loaded_g.requiredObls(intersectioncasePS).tail ++ loaded_g.requiredObls(differencecasePS).tail

    for ((o, e) <- unsuccessfulObls) {
      val stepO = loaded_g.appliedStep(o)
      assert(stepO.nonEmpty)
      val step = stepO.get
      val resO = loaded_g.verifiedBy(step)
      assert(resO.nonEmpty)
      val res = resO.get

      assert(!res.status.isVerified)
      assert(res.errorMsg.nonEmpty)
      assert(res.errorMsg.get == "Time limit")
      assert(res.evidence.isEmpty)
    }
  }

  test("Successfully verified case distinction steps in loaded graph") {
    val pslist = List(unioncasePS, intersectioncasePS, differencecasePS)
    val ress = pslist.map(loaded_g.verifiedBy(_))

    for (resO <- ress; res <- resO) {
      assert(res.status.isVerified)
      assert(res.errorMsg.isEmpty)
      assert(res.evidence.nonEmpty)
    }

    for (ps <- pslist) {
      assert(!ps.tactic.allRequiredOblsVerified(loaded_g)(unioncase, loaded_g.requiredObls(ps)))
    }

  }

  test("Verifying lemma application step selectFromWhere inconclusive (Time limit)") {
    val resO = loaded_g.verifiedBy(selcasePS)
    assert(resO.nonEmpty)
    val res = resO.get

    assert(!res.status.isVerified)
    assert(res.errorMsg.nonEmpty)
    assert(res.errorMsg.get == "Time limit")
    assert(res.evidence.isEmpty)
    assert(!selcasePS.tactic.allRequiredOblsVerified(loaded_g)(selcase, loaded_g.requiredObls(selcasePS)))
  }

  val selLemmaPS = loaded_g.appliedStep(selcase).get

  val successfulLookupobl = MockLemmaApplication.selectLemma(successfulLookup.lemmas.head.name,
    loaded_g.requiredObls(selLemmaPS))
  val successfulLookupPS = loaded_g.appliedStep(successfulLookupobl).get
  val successfulLookupbasecase = MockInduction.selectCase(successfulLookupEmpty.goals.head.name, loaded_g.requiredObls(successfulLookupPS))
  val successfulLookupstepcase = MockInduction.selectCase(successfulLookupBind.goals.head.name, loaded_g.requiredObls(successfulLookupPS))


  test("Successful verification of cases of successfulLookup") {
    val cases = List(successfulLookupbasecase, successfulLookupstepcase)
    val ress = for (c <- cases; ps <- loaded_g.appliedStep(c)) yield loaded_g.verifiedBy(ps)

    assert(ress.size == 2)
    for (resO <- ress) {
      assert(resO.nonEmpty)
      val res = resO.get
      assert(res.status.isVerified)
      assert(res.errorMsg.isEmpty)
      assert(res.evidence.nonEmpty)
    }

    for (c <- cases; ps <- loaded_g.appliedStep(c)) {
      assert(ps.tactic.allRequiredOblsVerified(loaded_g)(c, loaded_g.requiredObls(ps)))
    }
  }

  val welltypedLookupobl = MockLemmaApplication.selectLemma(welltypedLookup.lemmas.head.name,
    loaded_g.requiredObls(selLemmaPS))
  val welltypedLookupPS = loaded_g.appliedStep(welltypedLookupobl).get
  val welltypedLookupbasecase = MockInduction.selectCase(welltypedLookupEmpty.goals.head.name, loaded_g.requiredObls(welltypedLookupPS))
  val welltypedLookupstepcase = MockInduction.selectCase(welltypedLookupBind.goals.head.name, loaded_g.requiredObls(welltypedLookupPS))


  test("Successful verification of cases of welltypedLookup") {
    val cases = List(welltypedLookupbasecase, welltypedLookupstepcase)
    val ress = for (c <- cases; ps <- loaded_g.appliedStep(c)) yield loaded_g.verifiedBy(ps)

    assert(ress.size == 2)
    for (resO <- ress) {
      assert(resO.nonEmpty)
      val res = resO.get
      assert(res.status.isVerified)
      assert(res.errorMsg.isEmpty)
      assert(res.evidence.nonEmpty)
    }

    for (c <- cases; ps <- loaded_g.appliedStep(c)) {
      assert(ps.tactic.allRequiredOblsVerified(loaded_g)(c, loaded_g.requiredObls(ps)))
    }
  }

  val filterPreservesTypeobl = MockLemmaApplication.selectLemma(filterPreservesType.lemmas.head.name,
    loaded_g.requiredObls(selLemmaPS))
  val filterPreservesTypePS = loaded_g.appliedStep(filterPreservesTypeobl).get

  test("Successful Verification of filterPreservesType via auxiliary lemma") {

    val resO = loaded_g.verifiedBy(filterPreservesTypePS)
    assert(resO.nonEmpty)
    val res = resO.get
    assert(res.status.isVerified)
    assert(res.errorMsg.isEmpty)
    assert(res.evidence.nonEmpty)

    assert(!filterPreservesTypePS.tactic.allRequiredOblsVerified(loaded_g)(filterPreservesTypeobl,
      loaded_g.requiredObls(filterPreservesTypePS)))

  }

  val filterRowsPreservesTableObl = MockLemmaApplication.selectLemma(filterRowsPreservesTable.lemmas.head.name,
    loaded_g.requiredObls(filterPreservesTypePS))
  val filterRowsPreservesTablePS = loaded_g.appliedStep(filterRowsPreservesTableObl).get
  val filterRowsPreservesTablebasecase = MockInduction.selectCase(filterRowsPreservesTableTempty.goals.head.name,
    loaded_g.requiredObls(filterRowsPreservesTablePS))
  val filterRowsPreservesTablestepcase = MockInduction.selectCase(filterRowsPreservesTableTcons.goals.head.name,
    loaded_g.requiredObls(filterRowsPreservesTablePS))


  test("Verification of filterRowsPreservesTable cases (successful base ad step case)") {
    val cases = List(filterRowsPreservesTablebasecase, filterRowsPreservesTablestepcase)
    val casePSs = for (c <- cases; ps <- loaded_g.appliedStep(c)) yield ps
    val ress = for (ps <- casePSs) yield loaded_g.verifiedBy(ps)

    assert(ress.size == 2)
    assert(ress.forall(_.nonEmpty))
    val PSbase = casePSs.head
    val resbase = ress.head.get

    assert(resbase.status.isVerified)
    assert(resbase.errorMsg.isEmpty)
    assert(resbase.evidence.nonEmpty)
    assert(PSbase.tactic.allRequiredOblsVerified(loaded_g)
    (filterRowsPreservesTablebasecase, loaded_g.requiredObls(PSbase)))

    val PSstep = casePSs.last
    val resstep = ress.last.get

    assert(resstep.status.isVerified)
    assert(resstep.errorMsg.isEmpty)
    assert(resstep.evidence.nonEmpty)
  }

  val projectTableProgressobl = MockLemmaApplication.selectLemma(projectTableProgress.lemmas.head.name,
    loaded_g.requiredObls(selLemmaPS))
  val projectTableProgressPS = loaded_g.appliedStep(projectTableProgressobl).get


  test("Verification of projectTableProgress via auxiliary lemma projectColsProgress") {
    val resO = loaded_g.verifiedBy(projectTableProgressPS)
    assert(resO.nonEmpty)
    val res = resO.get
    assert(res.status.isVerified)
    assert(res.errorMsg.isEmpty)
    assert(res.evidence.nonEmpty)

    //induction application unverified
    assert(!projectTableProgressPS.tactic.allRequiredOblsVerified(loaded_g)(projectTableProgressobl,
      loaded_g.requiredObls(projectTableProgressPS)))
  }

  val projectColsProgressObl = MockLemmaApplication.selectLemma(projectColsProgress.lemmas.head.name,
    loaded_g.requiredObls(projectTableProgressPS))
  val projectColsProgressPS = loaded_g.appliedStep(projectColsProgressObl).get
  val projectColsProgressbasecase = MockInduction.selectCase(projectColsProgressAempty.goals.head.name,
    loaded_g.requiredObls(projectColsProgressPS))
  val projectColsProgressstepcase = MockInduction.selectCase(projectColsProgressAcons.goals.head.name,
    loaded_g.requiredObls(projectColsProgressPS))



  test("Verification of cases of projectColsProgress (successful base and step case)") {
    val cases = List(projectColsProgressbasecase, projectColsProgressstepcase)
    val casePSs = for (c <- cases; ps <- loaded_g.appliedStep(c)) yield ps
    val ress = for (ps <- casePSs) yield loaded_g.verifiedBy(ps)

    assert(ress.size == 2)
    assert(ress.forall(_.nonEmpty))
    val PSbase = casePSs.head
    val resbase = ress.head.get

    assert(resbase.status.isVerified)
    assert(resbase.errorMsg.isEmpty)
    assert(resbase.evidence.nonEmpty)
    assert(PSbase.tactic.allRequiredOblsVerified(loaded_g)
    (projectColsProgressbasecase, loaded_g.requiredObls(PSbase)))

    val PSstep = casePSs.last
    val resstep = ress.last.get

    assert(resstep.status.isVerified)
    assert(resstep.errorMsg.isEmpty)
    assert(resstep.evidence.nonEmpty)
  }

  val projectColsProgressstepcasePS = loaded_g.appliedStep(projectColsProgressstepcase).get
  val projectTypeImpliesFindColObl = MockLemmaApplication.selectLemma(projectTypeImpliesFindCol.lemmas.head.name,
    loaded_g.requiredObls(projectColsProgressstepcasePS))
  val projectTypeImpliesFindColPS = loaded_g.appliedStep(projectTypeImpliesFindColObl).get
  val projectTypeImpliesFindColbasecase = MockInduction.selectCase(projectTypeImpliesFindColAempty.goals.head.name,
    loaded_g.requiredObls(projectTypeImpliesFindColPS))
  val projectTypeImpliesFindColstepcase = MockInduction.selectCase(projectTypeImpliesFindColAcons.goals.head.name,
    loaded_g.requiredObls(projectTypeImpliesFindColPS))


  test("Verification of cases of projectTypeImpliesFindCol (successful base case, inconclusive step case)") {
    val cases = List(projectTypeImpliesFindColbasecase, projectTypeImpliesFindColstepcase)
    val casePSs = for (c <- cases; ps <- loaded_g.appliedStep(c)) yield ps
    val ress = for (ps <- casePSs) yield loaded_g.verifiedBy(ps)

    assert(ress.size == 2)
    assert(ress.forall(_.nonEmpty))
    val PSbase = casePSs.head
    val resbase = ress.head.get

    assert(resbase.status.isVerified)
    assert(resbase.errorMsg.isEmpty)
    assert(resbase.evidence.nonEmpty)
    assert(PSbase.tactic.allRequiredOblsVerified(loaded_g)
    (projectTypeImpliesFindColbasecase, loaded_g.requiredObls(PSbase)))

    val PSstep = casePSs.last
    val resstep = ress.last.get

    assert(!resstep.status.isVerified)
    assert(resstep.errorMsg.nonEmpty)
    assert(resstep.evidence.isEmpty)
  }

  val projectTypeImpliesFindColstepcasePS = loaded_g.appliedStep(projectTypeImpliesFindColstepcase).get
  val findColTypeImpliesfindColObl = MockLemmaApplication.selectLemma(findColTypeImpliesfindCol.lemmas.head.name,
    loaded_g.requiredObls(projectTypeImpliesFindColstepcasePS))
  val findColTypeImpliesfindColPS = loaded_g.appliedStep(findColTypeImpliesfindColObl).get
  val findColTypeImpliesfindColbasecase = MockInduction.selectCase(findColTypeImpliesfindColAempty.goals.head.name,
    loaded_g.requiredObls(findColTypeImpliesfindColPS))
  val findColTypeImpliesfindColstepcase = MockInduction.selectCase(findColTypeImpliesfindColAcons.goals.head.name,
    loaded_g.requiredObls(findColTypeImpliesfindColPS))

  test("Verification of cases of findColTypeImpliesfindCol (successful base case, inconclusive step case)") {
    val cases = List(findColTypeImpliesfindColbasecase, findColTypeImpliesfindColstepcase)
    val casePSs = for (c <- cases; ps <- loaded_g.appliedStep(c)) yield ps
    val ress = for (ps <- casePSs) yield loaded_g.verifiedBy(ps)

    assert(ress.size == 2)
    assert(ress.forall(_.nonEmpty))
    val PSbase = casePSs.head
    val resbase = ress.head.get

    assert(resbase.status.isVerified)
    assert(resbase.errorMsg.isEmpty)
    assert(resbase.evidence.nonEmpty)
    assert(PSbase.tactic.allRequiredOblsVerified(loaded_g)
    (findColTypeImpliesfindColbasecase, loaded_g.requiredObls(PSbase)))

    val PSstep = casePSs.last
    val resstep = ress.last.get

    assert(!resstep.status.isVerified)
    assert(resstep.errorMsg.nonEmpty)
    assert(resstep.evidence.isEmpty)
  }

  val projectTypeAttrLImpliesfindAllColTypeObl = MockLemmaApplication.selectLemma(projectTypeAttrLImpliesfindAllColType.lemmas.head.name,
    loaded_g.requiredObls(projectTypeImpliesFindColstepcasePS))
  val projectTypeAttrLImpliesfindAllColTypePS = loaded_g.appliedStep(projectTypeAttrLImpliesfindAllColTypeObl).get
  val projectTypeAttrLImpliesfindAllColTypebasecase = MockInduction.selectCase(projectTypeAttrLImpliesfindAllColTypeAempty.goals.head.name,
    loaded_g.requiredObls(projectTypeAttrLImpliesfindAllColTypePS))
  val projectTypeAttrLImpliesfindAllColTypestepcase = MockInduction.selectCase(projectTypeAttrLImpliesfindAllColTypeAcons.goals.head.name,
    loaded_g.requiredObls(projectTypeAttrLImpliesfindAllColTypePS))

  test("Verification of cases of projectTypeAttrLImpliesfindAllColType (successful base case, inconclusive step case)") {
    val cases = List(projectTypeAttrLImpliesfindAllColTypebasecase, projectTypeAttrLImpliesfindAllColTypestepcase)
    val ress = for (c <- cases; ps <- loaded_g.appliedStep(c)) yield loaded_g.verifiedBy(ps)

    assert(ress.size == 2)
    for (resO <- ress) {
      assert(resO.nonEmpty)
      val res = resO.get
      assert(res.status.isVerified)
      assert(res.errorMsg.isEmpty)
      assert(res.evidence.nonEmpty)
    }

    for (c <- cases; ps <- loaded_g.appliedStep(c)) {
      assert(ps.tactic.allRequiredOblsVerified(loaded_g)(c, loaded_g.requiredObls(ps)))
    }
  }

  val findColTypeImpliesfindColstepcasePS = loaded_g.appliedStep(findColTypeImpliesfindColstepcase).get
  val dropFirstColRawPreservesWelltypedRawObl = MockLemmaApplication.selectLemma(dropFirstColRawPreservesWelltypedRaw.lemmas.head.name,
    loaded_g.requiredObls(findColTypeImpliesfindColstepcasePS))
  val dropFirstColRawPreservesWelltypedRawPS = loaded_g.appliedStep(dropFirstColRawPreservesWelltypedRawObl).get
  val dropFirstColRawPreservesWelltypedRawbasecase = MockInduction.selectCase(dropFirstColRawPreservesWelltypedRawTempty.goals.head.name,
    loaded_g.requiredObls(dropFirstColRawPreservesWelltypedRawPS))
  val dropFirstColRawPreservesWelltypedRawstepcase = MockInduction.selectCase(dropFirstColRawPreservesWelltypedRawTcons.goals.head.name,
    loaded_g.requiredObls(dropFirstColRawPreservesWelltypedRawPS))

  test("Verification of cases of dropFirstColRawPreservesWelltypedRaw (successful base and step case)") {
    val cases = List(dropFirstColRawPreservesWelltypedRawbasecase, dropFirstColRawPreservesWelltypedRawstepcase)
    val casePSs = for (c <- cases; ps <- loaded_g.appliedStep(c)) yield ps
    val ress = for (ps <- casePSs) yield loaded_g.verifiedBy(ps)

    assert(ress.size == 2)
    assert(ress.forall(_.nonEmpty))
    val PSbase = casePSs.head
    val resbase = ress.head.get

    assert(resbase.status.isVerified)
    assert(resbase.errorMsg.isEmpty)
    assert(resbase.evidence.nonEmpty)
    assert(PSbase.tactic.allRequiredOblsVerified(loaded_g)
    (dropFirstColRawPreservesWelltypedRawbasecase, loaded_g.requiredObls(PSbase)))

    val PSstep = casePSs.last
    val resstep = ress.last.get
    println(resstep.status)

    assert(resstep.status.isVerified)
    assert(resstep.errorMsg.isEmpty)
    assert(resstep.evidence.nonEmpty)
  }

  test("ProofGraphUI proofstepDFS traverses in the correct order") {
    assert(loaded_g.obligationDFS().size == 41)
    val proofsteps = loaded_g.proofstepsDFS()
    assert(proofsteps.size == 41)
    assert(proofsteps(0).tactic == rootInductionProgress)
    assert(proofsteps(1).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(2).tactic.isInstanceOf[MockLemmaApplication])
    assert(proofsteps(3).tactic == successfulLookupInduction)
    assert(proofsteps(4).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(5).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(6).tactic == welltypedLookupInduction)
    assert(proofsteps(7).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(8).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(9).tactic.isInstanceOf[MockLemmaApplication])
    assert(proofsteps(10).tactic == filterRowsPreservesTableInduction)
    assert(proofsteps(11).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(12).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(13).tactic.isInstanceOf[MockLemmaApplication])
    assert(proofsteps(14).tactic == projectColsProgressInduction)
    assert(proofsteps(15).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(16).tactic.isInstanceOf[MockLemmaApplication])
    assert(proofsteps(17).tactic == projectTypeImpliesFindColInduction)
    assert(proofsteps(18).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(19).tactic.isInstanceOf[MockLemmaApplication])
    assert(proofsteps(20).tactic == findColTypeImpliesfindColInduction)
    assert(proofsteps(21).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(22).tactic.isInstanceOf[MockLemmaApplication])
    assert(proofsteps(23).tactic == dropFirstColRawPreservesWelltypedRawInduction)
    assert(proofsteps(24).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(25).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(26).tactic == projectTypeAttrLImpliesfindAllColTypeInduction)
    assert(proofsteps(27).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(28).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(29).tactic.isInstanceOf[SetCaseDistinction])
    assert(proofsteps(30).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(31).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(32).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(33).tactic.isInstanceOf[SetCaseDistinction])
    assert(proofsteps(34).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(35).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(36).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(37).tactic.isInstanceOf[SetCaseDistinction])
    assert(proofsteps(38).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(39).tactic == Solve[VeritasConstruct, VeritasConstruct]())
    assert(proofsteps(40).tactic == Solve[VeritasConstruct, VeritasConstruct]())
  }

  test("ProofGraphUI mapStepResult set every StepResult to Unknown") {
    val dbFile = File.createTempFile("test-newgraph", "")
    dbFile.delete()
    dbFile.mkdir()
    val newGraph = new ProofGraphXodus[VeritasConstruct, VeritasConstruct](dbFile) with ProofGraphTraversals[VeritasConstruct, VeritasConstruct]
    SQLSoundnessProofGraph.initializeGraphTypes(newGraph)
    loaded_g.mapStepResult(newGraph)(obl => newGraph.stepResultProducer.newStepResult(new Unknown(null), None, None))
    assert(loaded_g.proofstepsDFS().size == newGraph.proofstepsDFS().size)
    newGraph.proofstepsDFS().foreach { ps =>
      val stepresult = newGraph.verifiedBy(ps)
      assert(stepresult.nonEmpty)
      assert(stepresult.get.status.isInstanceOf[Unknown[_, _]])
    }
  }

  test("ProofGraphUI mapStepResult set Solve tactic stepresults to Unknown") {
    val dbFile = File.createTempFile("test-newgraph", "")
    dbFile.delete()
    dbFile.mkdir()
    val newGraph = new ProofGraphXodus[VeritasConstruct, VeritasConstruct](dbFile) with ProofGraphTraversals[VeritasConstruct, VeritasConstruct]
    SQLSoundnessProofGraph.initializeGraphTypes(newGraph)
    loaded_g.mapStepResult(newGraph){ obl =>
      val convertedObl = loaded_g.obligationProducer.newObligation(obl.spec, obl.goal)
      val ps = loaded_g.appliedStep(convertedObl)
      var newResult: Option[newGraph.StepResult] = None
      if (ps.nonEmpty) {
        if(ps.get.tactic.isInstanceOf[Solve[_,_]])
          newResult = Some(newGraph.stepResultProducer.newStepResult(new Unknown(null), None, None))
        else {
          val prevStepResult = loaded_g.verifiedBy(ps.get)
          if (prevStepResult.nonEmpty)
            newResult =
              Some(newGraph.stepResultProducer.newStepResult(
                prevStepResult.get.status, prevStepResult.get.evidence, prevStepResult.get.errorMsg))
        }
      }
      if(newResult.nonEmpty)
        newResult.get
      else
        newGraph.stepResultProducer.newStepResult(new Unknown(null), None, None)
    }
    newGraph.proofstepsDFS().foreach { ps =>
      val stepresult = newGraph.verifiedBy(ps)
      assert(stepresult.nonEmpty)
      if (ps.tactic.isInstanceOf[Solve[_,_]])
        assert(stepresult.get.status.isInstanceOf[Unknown[_, _]])
    }
  }

  test("Get obligations with zero subobligations") {
    val obls = loaded_g.leaves()
    assert(obls.size == 23)
  }

  test("Get obligation with 2 subobligations") {
    val obls = loaded_g.obligations(2);
    assert(obls.size == 9)

  }

  test("Get obligations with 3 subobligations") {
    val obls = loaded_g.obligations(3)
    assert(obls.size == 3)
  }

  test("Filter obligations based on type of applied step") {
    val obls = loaded_g.obligationsWithTactic[Solve[VeritasConstruct, VeritasConstruct]]
    assert(obls.size == 23)
    obls.foreach { obl =>
      val ps = loaded_g.appliedStep(obl)
      assert(ps.get.tactic.isInstanceOf[Solve[VeritasConstruct, VeritasConstruct]])
    }
  }

  test("Extract subgraph at root") {
    val obl = loaded_g.obligations(2).head
    val dbFile = File.createTempFile("test-newgraph", "")
    dbFile.delete()
    dbFile.mkdir()
    val newGraph = new ProofGraphXodus[VeritasConstruct, VeritasConstruct](dbFile) with ProofGraphTraversals[VeritasConstruct, VeritasConstruct]
    SQLSoundnessProofGraph.initializeGraphTypes(newGraph)
    loaded_g.extractSubgraph(loaded_g.storedObligations.head._2, newGraph)
    assert(newGraph.storedObligations.size == 1)
    val rootObl = newGraph.storedObligations.head._2

    val ps = newGraph.appliedStep(rootObl)
    assert(ps.nonEmpty)
    assert(newGraph.obligationDFS.size == 41)
    assert(newGraph.leaves.size == 23)
    assert(newGraph.obligations(2).size == 9)
    assert(loaded_g.obligations(3).size == 3)
  }

  test("Extract subgraph with only two subobligations") {
    val obl = loaded_g.obligations(2).head
    val dbFile = File.createTempFile("test-newgraph", "")
    dbFile.delete()
    dbFile.mkdir()
    val newGraph = new ProofGraphXodus[VeritasConstruct, VeritasConstruct](dbFile) with ProofGraphTraversals[VeritasConstruct, VeritasConstruct]
    SQLSoundnessProofGraph.initializeGraphTypes(newGraph)
    // because we get the DFS order the last obligation with two subobligations has only solve tactics based on structure of the graph
    loaded_g.extractSubgraph(loaded_g.obligations(2).last, newGraph)
    assert(newGraph.storedObligations.size == 1)
    val rootObl = newGraph.storedObligations.head._2
    val ps = newGraph.appliedStep(rootObl)
    assert(ps.nonEmpty)
    val subobligations = newGraph.requiredObls(ps.get)
    assert(subobligations.size == 2)
    subobligations.foreach { subobl =>
      val ps = newGraph.appliedStep(subobl._1)
      assert(ps.get.tactic.isInstanceOf[Solve[_,_]])
    }
  }
}
