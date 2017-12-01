package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.StructuralInduction
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier._
import de.tu_darmstadt.veritas.VerificationInfrastructure.visualizer.Dot
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.util.FreshNames
import org.scalatest.FunSuite

class SQLVerificationTests extends FunSuite {

  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }


  //  test("Check induction application steps only") {
  //    //construct a new test database with SQL progress proof graph
  //    val file = new File("SQLProgressProof-inductionsteps")
  //    if (file.exists()) recursivedelete(file)
  //    if (!file.mkdir()) sys.error("Could not create new store for SQLSoundnessProofGraph-inductionsteps.")
  //
  //    val SQLPG = new SQLSoundnessProofGraph(file)
  //    val pg = SQLPG.g //actual ProofGraphXodus instance
  //
  //    def printStepResult(res: pg.StepResult): String =
  //      res.status match {
  //        case Finished(stat, ver) =>
  //          stat match {
  //            case Proved(ie@InductionSchemeEvidence(_,_,_)) => s"Proved: \n ${ie.toString}."
  //            case Proved(_) => "Proved (unknown details)."
  //            case Disproved(_) => "Disproved."
  //            case Inconclusive(_) => "Inconclusive."
  //            case ProverFailure(_) => "Failure."
  //          }
  //        case Unknown(_) => "Unknown"
  //        case VerifierFailure(err, _) => "VerifierFailure :" + err
  //      }
  //
  //
  //    //only induction steps!
  //    val indobls = pg.obligationDFS() filter (o => pg.appliedStep(o).get.tactic.isInstanceOf[StructuralInduction[VeritasConstruct, VeritasFormula]])
  //    val indps = for (obl <- indobls) yield pg.appliedStep(obl).get
  //
  //    val indver = new TrustInductionSchemeVerifier[VeritasConstruct, VeritasFormula]()
  //
  //    for (ps <- indps)
  //      println(printStepResult(pg.verifyProofStep(ps, indver, None)))
  //  }

  test("Manual step simplification set cases") {
    val file = new File("SQLProgressProof-manualsteps")
    if (file.exists()) recursivedelete(file)
    if (!file.mkdir()) sys.error("Could not create new store for SQLSoundnessProofGraph-allsteps.")

    val SQLPG = new SQLSoundnessProofGraph(file)
    val pg = SQLPG.g //actual ProofGraphXodus instance

    val SQLPG_UI = new ProofGraphUI[VeritasConstruct, VeritasFormula](pg, ProofGraphUI.extractGoalOrLemmaName)

    val testobl = SQLPG_UI.getObligation("SQL-Progress-icase2-case1")
    val edgetotestobl = SQLPG_UI.pg.requiringSteps(testobl)

    //TODO: apply manual simplification steps to the obligation, test whether the steps can be verified

    assert(true)
  }

  test("Construct and visualize complete graph") {
    //construct a new test database with SQL progress proof graph
    val file = new File("SQLProgressProof-allsteps")
    if (file.exists()) recursivedelete(file)
    if (!file.mkdir()) sys.error("Could not create new store for SQLSoundnessProofGraph-allsteps.")

    val SQLPG = new SQLSoundnessProofGraph(file)
    val pg = SQLPG.g //actual ProofGraphXodus instance

    def makeCustomVampire(timeout: Int, logic: String) = new TPTPVampireVerifier(timeout, "4.1", logic)

    def printStepResult(res: pg.StepResult): String =
      res.status match {
        case Finished(stat, ver) =>
          stat match {
            case Proved(ie@InductionSchemeEvidence(_, _, _)) => s"Proved (induction scheme application)."
            case Proved(ATPResultDetails(_, _, _, _, Some(t))) => "Proved (" + t + "s)."
            case Proved(_) => "Proved (unknown details)."
            case Disproved(_) => "Disproved."
            case Inconclusive(_) => "Inconclusive."
            case ProverFailure(_) => "Failure."
          }
        case Unknown(_) => "Unknown"
        case VerifierFailure(err, _) => "VerifierFailure :" + err
      }

    val fresh = new FreshNames()

    //test

    def extractGoalName(vc: VeritasConstruct): String =
      vc match {
        case Goals(gl, _) => gl.head.name
        case Lemmas(ll, _) => ll.head.name
        case Axioms(axl) => axl.head.name
        case TypingRule(name, _, _) => name
        case _ => fresh.freshRuleName("obl_")
      }

    //only induction steps!
    val (indobls, noindobls) = pg.obligationDFS() partition (o => pg.appliedStep(o).get.tactic.isInstanceOf[StructuralInduction[VeritasConstruct, VeritasFormula]])

    val indver = new TrustInductionSchemeVerifier[VeritasConstruct, VeritasFormula]()
    val noindver = makeCustomVampire(90, "fof")

    for (obl <- indobls) {
      val ps = pg.appliedStep(obl).get
      val goalname = extractGoalName(obl.goal)
      println(s"$goalname: " + printStepResult(pg.verifyProofStep(ps, indver, None)))
    }

    for (obl <- noindobls) {
      val ps = pg.appliedStep(obl).get
      val goalname = extractGoalName(obl.goal)
      println(s"$goalname: " + printStepResult(pg.verifyProofStep(ps, noindver, None)))
    }

    //visualize proof graph

    def visualizeGraph(filename: String) {
      val graphfile = new File(filename)
      if (file.exists()) recursivedelete(file)
      Dot(pg, graphfile)
    }

    visualizeGraph("SQLProgressCompleteVerification.png")


  }

}
