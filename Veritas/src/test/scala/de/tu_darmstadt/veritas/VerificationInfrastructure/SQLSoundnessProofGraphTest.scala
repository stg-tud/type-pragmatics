package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.CaseDistinctionCase
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


  //test("Timeout for individual set cases (SQL progress proof) in loaded graph") {

  //}
  //
  //  //  test("Proving a single set case") {
  //  //    val simpleVerifier = new TPTPVampireVerifier(30, "4.0")
  //  //
  //  //    val result = g.verifyProofStep(setPS.head, simpleVerifier)
  //  //
  //  //    println(result.status)
  //  //    assert(result.status.isInstanceOf[Finished[_, _]])
  //  //    assert(result.status.isVerified)
  //  //  }
  //  //
  //  //  test("Proving all individual set cases with Vampire 4.0") {
  //  //    val simpleVerifier = new TPTPVampireVerifier(30, "4.0")
  //  //
  //  //    for (ps <- setPS) {
  //  //      val result = g.verifyProofStep(ps, simpleVerifier)
  //  //
  //  //      assert(result.status.isInstanceOf[Finished[_, _]])
  //  //      assert(result.status.isVerified)
  //  //      assert(result.errorMsg.isEmpty)
  //  //      assert(result.evidence.nonEmpty)
  //  //    }
  //  //  }
  //
  //  test("Proving Case Distinction steps, Vampire 4.1, 5 seconds") {
  //    val simpleVerifier = new TPTPVampireVerifier(5)
  //
  //    val res1 = g.verifyProofStep(unioncasePS, simpleVerifier)
  //    val res2 = g.verifyProofStep(intersectioncasePS, simpleVerifier)
  //    val res3 = g.verifyProofStep(differencecasePS, simpleVerifier)
  //
  //
  //    assert(res1.status.isInstanceOf[Finished[_, _]])
  //    assert(res1.status.isVerified)
  //    assert(res1.errorMsg.isEmpty)
  //    assert(res1.evidence.nonEmpty)
  //
  //    assert(res2.status.isInstanceOf[Finished[_, _]])
  //    assert(res2.status.isVerified)
  //    assert(res2.errorMsg.isEmpty)
  //    assert(res2.evidence.nonEmpty)
  //
  //    assert(res3.status.isInstanceOf[Finished[_, _]])
  //    assert(res3.status.isVerified)
  //    assert(res3.errorMsg.isEmpty)
  //    assert(res3.evidence.nonEmpty)
  //  }
  //
  //
  //
  //
  //  test("Verify lemma application step (inconclusive)") {
  //    val simpleVerifier = new TPTPVampireVerifier(3)
  //
  //    val result = g.verifyProofStep(selLemmaPS, simpleVerifier)
  //
  //    //println(result.status)
  //    assert(result.status.isInstanceOf[Finished[_, _]])
  //
  //  }
  //
  //
  //  test("Verify cases of successfulLookup (Vampire 4.1)") {
  //    val simpleVerifier = new TPTPVampireVerifier(5)
  //
  //    val resbase = g.verifyProofStep(successfulLookupbasecasePL, simpleVerifier)
  //    val resstep = g.verifyProofStep(successfulLookupstepcasePL, simpleVerifier)
  //
  //    assert(resbase.status.isInstanceOf[Finished[_, _]])
  //    assert(resbase.status.isVerified)
  //    assert(resbase.errorMsg.isEmpty)
  //    assert(resbase.evidence.nonEmpty)
  //
  //    assert(resstep.status.isInstanceOf[Finished[_, _]])
  //    assert(resstep.status.isVerified)
  //    assert(resstep.errorMsg.isEmpty)
  //    assert(resstep.evidence.nonEmpty)
  //
  //  }
  //
  //
  //
  //  test("Verify cases of welltypedLookup (Vampire 4.1)") {
  //    val simpleVerifier = new TPTPVampireVerifier(5)
  //
  //    val resbase = g.verifyProofStep(welltypedLookupbasecasePS, simpleVerifier)
  //    val resstep = g.verifyProofStep(welltypedLookupstepcasePS, simpleVerifier)
  //
  //    assert(resbase.status.isInstanceOf[Finished[_, _]])
  //    assert(resbase.status.isVerified)
  //    assert(resbase.errorMsg.isEmpty)
  //    assert(resbase.evidence.nonEmpty)
  //
  //    assert(resstep.status.isInstanceOf[Finished[_, _]])
  //    assert(resstep.status.isVerified)
  //    assert(resstep.errorMsg.isEmpty)
  //    assert(resstep.evidence.nonEmpty)
  //
  //  }
  //
  //
  //  test("Verify filterPreservesType via auxiliary lemma (Vampire 4.1)") {
  //    val simpleVerifier = new TPTPVampireVerifier(5)
  //
  //    val result = g.verifyProofStep(filterPreservesTypePS, simpleVerifier)
  //
  //    assert(result.status.isInstanceOf[Finished[_, _]])
  //    assert(result.status.isVerified)
  //    assert(result.errorMsg.isEmpty)
  //    assert(result.evidence.nonEmpty)
  //
  //  }
  //
  //
  //
  //  test("Verify cases of filterRowsPreservesTable (Vampire 4.1)") {
  //    val simpleVerifier = new TPTPVampireVerifier(5)
  //
  //    val resbase = g.verifyProofStep(filterRowsPreservesTablebasecasePS, simpleVerifier)
  //    val resstep = g.verifyProofStep(filterRowsPreservesTablestepcasePS, simpleVerifier)
  //
  //    assert(resbase.status.isInstanceOf[Finished[_, _]])
  //    assert(resbase.status.isVerified)
  //    assert(resbase.errorMsg.isEmpty)
  //    assert(resbase.evidence.nonEmpty)
  //
  //    assert(resstep.status.isInstanceOf[Finished[_, _]])
  //    assert(resstep.status.isVerified)
  //    assert(resstep.errorMsg.isEmpty)
  //    assert(resstep.evidence.nonEmpty)
  //
  //  }
  //
  //
  //
  //
  //  test("Verify projectTableProgress via auxiliary lemma (Vampire 4.1)") {
  //    val simpleVerifier = new TPTPVampireVerifier(20)
  //
  //    val result = g.verifyProofStep(projectTableProgressPS, simpleVerifier)
  //
  //    println(result.status)
  //    assert(result.status.isInstanceOf[Finished[_, _]])
  //    assert(result.status.isVerified)
  //    assert(result.errorMsg.isEmpty)
  //    assert(result.evidence.nonEmpty)
  //
  //  }
  //
  //
  //
  //
  //  test("Verify cases of projectColsProgress (Vampire 4.1)") {
  //    val simpleVerifier = new TPTPVampireVerifier(5)
  //
  //    val resbase = g.verifyProofStep(projectColsProgressbasecasePS, simpleVerifier)
  //    val resstep = g.verifyProofStep(projectColsProgressstepcasePS, simpleVerifier)
  //
  //    assert(resbase.status.isInstanceOf[Finished[_, _]])
  //    assert(resbase.status.isVerified)
  //    assert(resbase.errorMsg.isEmpty)
  //    assert(resbase.evidence.nonEmpty)
  //
  //    assert(resstep.status.isInstanceOf[Finished[_, _]])
  //    assert(resstep.status.isVerified)
  //    assert(resstep.errorMsg.isEmpty)
  //    assert(resstep.evidence.nonEmpty)
  //
  //  }


}
