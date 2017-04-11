package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.CaseDistinctionCase
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.VerifierFailure
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

  val loaded_g: ProofGraphXodus[VeritasConstruct, VeritasConstruct] =
    new ProofGraphXodus[VeritasConstruct, VeritasConstruct](file)

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

  test("Successfully proven individual set cases (tvalue+tvalue) in loaded graph") {
    val successfulObls = List(loaded_g.requiredObls(unioncasePS).head,
      loaded_g.requiredObls(intersectioncasePS).head, loaded_g.requiredObls(differencecasePS).head)

    for ((o, e) <- successfulObls) {
      val stepO = loaded_g.appliedStep(o)
      assert(stepO.nonEmpty)
      val step = stepO.get
      val resO = loaded_g.verifiedBy(step)
      assert(resO.nonEmpty)
      val res = resO.get

      assert(res.status.isVerified)
      assert(res.errorMsg.isEmpty)
      assert(res.evidence.nonEmpty)
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


  test("Verification of filterRowsPreservesTable cases (successful base case, inconclusive step case)") {
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

    assert(!resstep.status.isVerified)
    assert(resstep.errorMsg.nonEmpty)
    assert(resstep.evidence.isEmpty)
  }

  val projectTableProgressobl = MockLemmaApplication.selectLemma(projectTableProgress.lemmas.head.name,
    loaded_g.requiredObls(selLemmaPS))
  val projectTableProgressPS = loaded_g.appliedStep(projectTableProgressobl).get


  test("Incomplete verification of projectTableProgress via auxiliary lemma projectColsProgress") {
    val resO = loaded_g.verifiedBy(projectTableProgressPS)
    assert(resO.nonEmpty)
    val res = resO.get
    assert(!res.status.isVerified)
    assert(res.errorMsg.nonEmpty)
    assert(res.evidence.isEmpty)

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



  test("Verification of cases of projectColsProgress (successful base case, inconclusive step case)") {
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

    assert(!resstep.status.isVerified)
    assert(resstep.errorMsg.nonEmpty)
    assert(resstep.evidence.isEmpty)
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

  test("Verification of cases of dropFirstColRawPreservesWelltypedRaw (successful base case, inconclusive step case)") {
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

    //for this step, there was a type inference error in loaded graph!
    assert(!resstep.status.isVerified)
    assert(resstep.status.isInstanceOf[VerifierFailure[_,_]])
    assert(resstep.errorMsg.isEmpty)
    assert(resstep.evidence.isEmpty)
  }

}
