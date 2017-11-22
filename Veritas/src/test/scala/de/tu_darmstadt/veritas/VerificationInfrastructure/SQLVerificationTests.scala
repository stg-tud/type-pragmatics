package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.StructuralInduction
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier._
import de.tu_darmstadt.veritas.backend.ast.{VeritasConstruct, VeritasFormula}
import org.scalatest.FunSuite

class SQLVerificationTests extends FunSuite {

  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }


  test("Check induction application steps only") {
    //construct a new test database with SQL progress proof graph
    val file = new File("SQLProgressProof-comparison")
    if (file.exists()) recursivedelete(file)
    if (!file.mkdir()) sys.error("Could not create new store for SQLSoundnessProofGraph-comparison.")

    val SQLPG = new SQLSoundnessProofGraph(file)
    val pg = SQLPG.g //actual ProofGraphXodus instance

    def printStepResult(res: pg.StepResult): String =
      res.status match {
        case Finished(stat, ver) =>
          stat match {
            case Proved(ie@InductionSchemeEvidence(_,_,_)) => s"Proved: \n ${ie.toString}."
            case Proved(_) => "Proved (unknown details)."
            case Disproved(_) => "Disproved."
            case Inconclusive(_) => "Inconclusive."
            case ProverFailure(_) => "Failure."
          }
        case Unknown(_) => "Unknown"
        case VerifierFailure(err, _) => "VerifierFailure :" + err
      }


    //only induction steps!
    val indobls = pg.obligationDFS() filter (o => pg.appliedStep(o).get.tactic.isInstanceOf[StructuralInduction[VeritasConstruct, VeritasFormula]])
    val indps = for (obl <- indobls) yield pg.appliedStep(obl).get

    val indver = new TrustInductionSchemeVerifier[VeritasConstruct, VeritasFormula]()

    for (ps <- indps)
      println(printStepResult(pg.verifyProofStep(ps, indver, None)))
  }

}
