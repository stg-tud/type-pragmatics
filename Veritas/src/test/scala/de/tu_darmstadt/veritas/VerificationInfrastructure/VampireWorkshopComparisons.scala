package de.tu_darmstadt.veritas.VerificationInfrastructure

import org.scalatest.FunSuite
import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.SQLMockTactics.MockInduction
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.util.FreshNames

class VampireWorkshopComparisons extends FunSuite {

  val fresh = new FreshNames()

  def extractGoalName(vc: VeritasConstruct): String =
    vc match {
      case Goals(gl, _) => gl.head.name
      case Lemmas(ll, _) => ll.head.name
      case Axioms(axl) => axl.head.name
      case TypingRule(name, _, _) => name
      case _ => fresh.freshRuleName("obl_")
    }

  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }

  val defaultshort_timeout = 5
  val defaultlong_timeout = 120
  val unsuccessful_timeout = 1

  val timeout_queue = Seq(5, 10, 30, 90)

  def makeCustomVampireTar(timeout: Int) = new ADTVampireVerifier(timeout)
  def makeCustomVampireTarQueue: Seq[Verifier[VeritasConstruct, VeritasConstruct]] =
    for (t <- timeout_queue) yield makeCustomVampireTar(t)

  def makeCustomVampireZ3(timeout: Int) = new Z3VampireVerifier(timeout)
  def makeCustomVampireZ3Queue: Seq[Verifier[VeritasConstruct, VeritasConstruct]] =
    for (t <- timeout_queue) yield makeCustomVampireZ3(t)

  def makeCustomVampire(timeout: Int, logic: String) = new TPTPVampireVerifier(timeout, "4.1", logic)
  def makeCustomVampireQueue(logic: String): Seq[Verifier[VeritasConstruct, VeritasConstruct]] =
    for (t <- timeout_queue) yield makeCustomVampire(t, logic)

  test("Compare verification of SQL progress proof goals") {
    //construct a new test database with SQL progress proof graph
    val file = new File("SQLProgressProof-comparison")
    if (file.exists()) recursivedelete(file)
    if (!file.mkdir()) sys.error("Could not create new store for SQLSoundnessProofGraph-comparison.")

    val SQLPG = new SQLSoundnessProofGraph(file)
    val pg = SQLPG.g //actual ProofGraphXodus instance

    val noinductobls = pg.obligationDFS() filter (o => !pg.appliedStep(o).get.tactic.isInstanceOf[MockInduction])
    val solveps = for (obl <- noinductobls) pg.appliedStep(obl)

    println("Comparison results: ")
    println("Goalname; Vampire 4.1 FOF; Vampire 4.1 TFF; Vampire 4.1 tar SMTLIB; VampireZ3 4.1 tar SMTLIB")

    def printStepResult(res: pg.StepResult): String =
      res.status match {
        case Finished(stat, ver) =>
          stat match {
            case Proved(ATPResultDetails(_, _, _, _, Some(t))) => "Proved (" + t + "s)."
            case Proved(_) => "Proved (unknown prover time)."
            case Disproved(_) => "Disproved."
            case Inconclusive(_) => "Inconclusive."
            case ProverFailure(_) => "Failure."
          }
        case Unknown(_) => "Unknown"
        case VerifierFailure(err, _) => "VerifierFailure :" + err
      }

    def tryVerifyingWithIncreasingTimeouts(ps: pg.ProofStep, vers: Seq[Verifier[VeritasConstruct, VeritasConstruct]]): pg.StepResult = {
      lazy val results =
        for (ver <- vers) yield {
          pg.verifyProofStep(ps, ver)
        }
      val maybefinal = results.find { sr =>
        sr.status match {
          case Finished(Proved(_), _) => true
          case _ => false
        }
      }
      maybefinal.getOrElse(results.last)
    }

      for (obl <- noinductobls) {
        val ps = pg.appliedStep(obl).get
        val goalname = extractGoalName(obl.goal)
        val vampire_4_1_fof = tryVerifyingWithIncreasingTimeouts(ps, makeCustomVampireQueue("fof"))
        val vampire_4_1_tff = tryVerifyingWithIncreasingTimeouts(ps, makeCustomVampireQueue("tff"))
        val vampire_4_1_tar = tryVerifyingWithIncreasingTimeouts(ps, makeCustomVampireTarQueue)
        val vampire_4_1_Z3_tar = tryVerifyingWithIncreasingTimeouts(ps, makeCustomVampireZ3Queue)

        val vampire_4_1_fof_res = printStepResult(vampire_4_1_fof)
        val vampire_4_1_tff_res = printStepResult(vampire_4_1_tff)
        val vampire_4_1_tar_res = printStepResult(vampire_4_1_tar)
        val vampire_4_1_Z3_tar_res = printStepResult(vampire_4_1_Z3_tar)

        println(s"$goalname; $vampire_4_1_fof_res; $vampire_4_1_tff_res; $vampire_4_1_tar_res; $vampire_4_1_Z3_tar_res")
      }
    }

  }
