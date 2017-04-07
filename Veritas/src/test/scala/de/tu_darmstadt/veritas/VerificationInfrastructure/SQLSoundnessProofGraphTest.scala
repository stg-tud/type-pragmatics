package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.{CaseDistinctionCase, Solve}
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{Finished, TPTPVampireVerifier}
import de.tu_darmstadt.veritas.backend.ast.{Goals, Lemmas, Module, VeritasConstruct}
import de.tu_darmstadt.veritas.inputdsl.{DataTypeDSL, FunctionDSL, SymTreeDSL}
import org.scalatest.FunSuite


/**
  * Constructing the SQL soundness proof graph and tests on the graph
  */
class SQLSoundnessProofGraphTest extends FunSuite {

  //Tries to load existing SQLSoundnessGraph
  val file = new File("SQLSoundnessProofGraph-store")
  if (!file.isDirectory) {
    sys.error("Test store SQLSoundnessProofGraph-store does not exist.")
  }

  val loaded_g: ProofGraphXodus[VeritasConstruct, VeritasConstruct] =
    new ProofGraphXodus[VeritasConstruct, VeritasConstruct](file)

  SQLSoundnessProofGraph.initializeGraphTypes(loaded_g)


  test("Graph was loaded correctly and has the expected number of obligations") {
    assert(loaded_g.storedObligations.size == 1)

    val rootobl = loaded_g.findObligation("SQL Progress")
    assert(rootobl.nonEmpty)
  }


  import DataTypeDSL._
  import FunctionDSL._
  import SymTreeDSL._
  import de.tu_darmstadt.veritas.inputdsl.TypingRuleDSL._
  import de.tu_darmstadt.veritas.inputdsl.ProofDSL._

  import de.tu_darmstadt.veritas.inputdsl.SQLDefs._


  // We instantiate S = VeritasConstruct (should be a Module) and P = VeritasConstruct (Should be Goal/local)


  //  val testGoal: Goals = goal(===>("test")('p ('x) && 'q ('x) || 't ('x)))
  //  val testObligation: g.Obligation = g.newObligation(Module("empty", Seq(), Seq()), testGoal)
  //
  //  test("Storing and finding the test obligation") {
  //    g.storeObligation("test", testObligation)
  //
  //    val r = g.findObligation("test")
  //
  //    assert(r.get.spec == testObligation.spec)
  //    assert(r.get.goal == testObligation.goal)
  //  }
  //
  //  test("Unstoring the test obligation") {
  //
  //    //first check whether obligation is still there
  //    val ro = g.findObligation("test")
  //
  //    assert(ro.get.spec == testObligation.spec)
  //    assert(ro.get.goal == testObligation.goal)
  //
  //    g.unstoreObligation(testObligation)
  //    val r = g.findObligation("test")
  //
  //    assert(r == None)
  //  }


  //  test("Storing and finding the progress obligation") {
  //    val r = g.findObligation("SQL progress")
  //
  //    assert(r.get.spec == progressObligation.spec)
  //    assert(r.get.goal == progressObligation.goal)
  //  }
  //
  //
  //
  //  test("All root induction cases are retrievable") {
  //    val obls = g.requiredObls(rootinductionPS)
  //    val tvaluecase = MockInduction.selectCase(SQLProgressTtvalue.goals.head.name, obls)
  //    val selcase = MockInduction.selectCase(SQLProgressTselectFromWhere.goals.head.name, obls)
  //    val unioncase = MockInduction.selectCase(SQLProgressTUnion.goals.head.name, obls)
  //    val intersectioncase = MockInduction.selectCase(SQLProgressTIntersection.goals.head.name, obls)
  //    val differencecase = MockInduction.selectCase(SQLProgressTDifference.goals.head.name, obls)
  //
  //    assert(tvaluecase.spec == fullSQLspec)
  //    assert(tvaluecase.goal == SQLProgressTtvalue)
  //
  //    assert(selcase.spec == fullSQLspec)
  //    assert(selcase.goal == SQLProgressTselectFromWhere)
  //
  //    assert(unioncase.spec == fullSQLspec)
  //    assert(unioncase.goal == SQLProgressTUnion)
  //
  //    assert(intersectioncase.spec == fullSQLspec)
  //    assert(intersectioncase.goal == SQLProgressTIntersection)
  //
  //    assert(differencecase.spec == fullSQLspec)
  //    assert(differencecase.goal == SQLProgressTDifference)
  //  }
  //
  //
  //  test("T-value base case is provable (using Vampire 4.1, 5 sec)") {
  //    val simpleVerifier = new TPTPVampireVerifier(5)
  //
  //    val result = g.verifyProofStep(tvaluecasePS, simpleVerifier)
  //    assert(result.status.isInstanceOf[Finished[_, _]])
  //    assert(result.status.isVerified)
  //    assert(result.errorMsg.isEmpty)
  //    assert(result.evidence.nonEmpty)
  //
  //  }
  //
  //
  //  test("Applying case distinctions worked as desired") {
  //    val unionobls = g.requiredObls(unioncasePS)
  //    val union1 = MockCaseDistinction.selectCase("Union1", unionobls)
  //    val intersectionobls = g.requiredObls(intersectioncasePS)
  //    val intersection3 = MockCaseDistinction.selectCase("Intersection3", intersectionobls)
  //    val differenceobls = g.requiredObls(differencecasePS)
  //    val difference2 = MockCaseDistinction.selectCase("Difference2", differenceobls)
  //
  //    assert(union1.goal == mkSQLProgressTSetCase(0, unionsym, sunion, case1pred))
  //    assert(intersection3.goal == mkSQLProgressTSetCase(2, intersectionsym, sintersection, case3pred))
  //    assert(difference2.goal == mkSQLProgressTSetCase(1, differencesym, sdifference, case2pred))
  //
  //    //compare some induction hypotheses
  //
  //    val obls = g.requiredObls(rootinductionPS)
  //    val unioncase = MockInduction.selectCase(SQLProgressTUnion.goals.head.name, obls)
  //    val intersectioncase = MockInduction.selectCase(SQLProgressTIntersection.goals.head.name, obls)
  //    val differencecase = MockInduction.selectCase(SQLProgressTDifference.goals.head.name, obls)
  //
  //    val unionps = g.appliedStep(unioncase).get
  //    val unionedges = g.requiredObls(unionps).toSeq
  //    assert(unionedges.size == 3)
  //    assert(unionedges(1)._2.asInstanceOf[CaseDistinctionCase[VeritasConstruct]].ihs.ihs == SQLProgressTUnionIH2)
  //
  //  }
  //
  //
  //
  //  test("Timeout for individual set cases (SQL progress proof), using Vampire 4.1, 1 sec") {
  //    val simpleVerifier = new TPTPVampireVerifier(1)
  //
  //    for (ps <- setPS) {
  //      val result = g.verifyProofStep(ps, simpleVerifier)
  //
  //      assert(result.status.isInstanceOf[Finished[_, _]])
  //      assert(result.errorMsg.nonEmpty)
  //      assert(result.errorMsg.get == "Time limit")
  //    }
  //  }
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
