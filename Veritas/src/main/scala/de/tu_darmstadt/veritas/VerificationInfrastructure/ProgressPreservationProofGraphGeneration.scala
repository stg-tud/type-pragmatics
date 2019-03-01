package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.strategies.VeritasProgressPreservationTopLevelStrategy
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.StructuralInduction
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier._
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.util.FreshNames

import scala.util.Success

// utility class with top-level methods for generating proof graphs for progress and preservation proofs,
// verifying them, visualizing them etc.
case class ProgressPreservationProofGraphGeneration(sourcepath: String, storepath: String) {

  // create separate folder for collecting the inconclusive problem descriptions
  private val log_inconclusive_problems_path: String = storepath + "-inconclusive"

  private val file_inconclusive = new File(log_inconclusive_problems_path)
  if (file_inconclusive.exists()) recursivedelete(file_inconclusive)
  if (!file_inconclusive.mkdir()) sys.error(s"Could not create new folder for $log_inconclusive_problems_path.")

  // helper variables
  val ppstrat = new VeritasProgressPreservationTopLevelStrategy(sourcepath, storepath)

  val pg_gen = ppstrat.generateFullGraph()

  val pg = ppstrat.PG_UI.pg


  private val fresh = new FreshNames()

  //helper functions (put in a utility class?)
  private def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete)
    file.delete
  }


  private def makeCustomVampire(timeout: Int, logic: String) = new TPTPVampireVerifier(timeout, "4.1", logic)


  private def extractGoalName(vc: VeritasConstruct): String =
    vc match {
      case Goals(gl, _) => gl.head.name
      case Lemmas(ll, _) => ll.head.name
      case Axioms(axl) => axl.head.name
      case TypingRule(name, _, _) => name
      case _ => fresh.freshRuleName("obl_")
    }

  private def writeToFile(filehandler: File, s: String) = {
    if (!filehandler.getParentFile.exists())
      filehandler.getParentFile.mkdirs()
    filehandler.createNewFile()
    new PrintWriter(filehandler) {
      write(s);
      close
    }
  }


  // print step results on console and log inconclusive problem descriptions as files
  private def printStepResult(ps: pg.ProofStep, res: pg.StepResult, log_inconclusive: Boolean = true): String =
    res.status match {
      case Finished(stat, ver) =>
        stat match {
          case Proved(ie@InductionSchemeEvidence(_, _, _)) => s"Proved (induction scheme application)."
          case Proved(ATPResultDetails(_, _, _, Some(lems), Some(t))) => s"Proved ($t s). Used lemmas: $lems"
          case Proved(_) => "Proved (unknown details)."
          case Disproved(_) => "Disproved."
          case Inconclusive(_) => {
            // write pretty printed version of assembled proof problem to a file
            if (log_inconclusive) {
              val goalobl = pg.targetedObl(ps)
              val goalname = extractGoalName(goalobl.goal)

              //val goalobl_ui = ppstrat.PG_UI.getObligation(goalname)

              val trans = new VeritasTransformer[TPTP](
                VeritasTransformerBestStrat.config, x => x.asInstanceOf[TPTP])

              //write pretty-printed version of problem to file
              //val (aspec, assms, agoal) = ppstrat.PG_UI.getAssembledProblem[TPTP](goalobl, trans)
              val parentedges = pg.requiringSteps(goalobl) map (_._2)
              val assumptions = pg.requiredObls(ps) map { case (o, el) => (el, o.goal) }
              val (aspec, assms, agoal) = trans.assembleFullProblem(goalobl.goal, goalobl.spec, parentedges, assumptions)


              val assembledproblem_str = s"DEFINITIONS: \n ${aspec.toPrettyString()} \n\n " +
                s"GOAL-SPECIFIC ASSUMPTIONS: \n ${assms.toPrettyString()} \n\n" +
                s"GOAL: \n ${agoal.toPrettyString()}"
              val assembledProblemFile = new File(s"$log_inconclusive_problems_path/$goalname-assembledProblem")
              writeToFile(assembledProblemFile, assembledproblem_str)

              //write translated version of problem to file
              val transformedProblem = trans.translateProblem((aspec, assms, agoal))
              val translatedProblemFile = new File(s"$log_inconclusive_problems_path/$goalname-translatedProblem")
              writeToFile(translatedProblemFile, transformedProblem.get.toString)
            }

            "Inconclusive."
          }
          case ProverFailure(_) => "Failure."
        }
      case Unknown(_) => "Unknown"
      case VerifierFailure(err, _) => "VerifierFailure :" + err
    }


  def visualizeProofGraph(filename: String = "GeneratedProofGraph.png"): Unit =
    ppstrat.visualizeGraph(filename)

  def printAllObligations(): Unit = ppstrat.printAllObligations()

  def verifyAll(log_inconclusive: Boolean = true, provertimeout: Int = 10) = {
    val indver = new TrustInductionSchemeVerifier[VeritasConstruct, VeritasFormula]()
    //use Vampire (4.1) with 120 sec timeout and tff encoding for verifying all steps that are not induction scheme applications
    val noindver = makeCustomVampire(provertimeout, "tff")

    val (indobls, noindobls) = pg.obligationDFS() partition (o => pg.appliedStep(o).get.tactic.isInstanceOf[StructuralInduction[VeritasConstruct, VeritasFormula]])


    //logging on console and potentially saving problems that cannot be proved to extra files (if log_inconclusive is true):
    for (obl <- indobls) {
      val ps = pg.appliedStep(obl).get
      val goalname = extractGoalName(obl.goal)
      println(s"$goalname: " + printStepResult(ps, pg.verifyProofStep(ps, indver, None), log_inconclusive))
    }

    for (obl <- noindobls) {
      val ps = pg.appliedStep(obl).get
      val goalname = extractGoalName(obl.goal)
      println(s"$goalname: " + printStepResult(ps, pg.verifyProofStep(ps, noindver, None), log_inconclusive))
    }


  }

  def checkConsistencyAll(log_inconsistent: Boolean = true, provertimeout: Int = 300): Unit = {
    val ver = makeCustomVampire(provertimeout, "tff")
    val consistencyChecker = ConsistencyChecker(VeritasTransformerBestStrat.config, ver.prover)

    val noindobls = pg.obligationDFS() filterNot (o => pg.appliedStep(o).get.tactic.isInstanceOf[StructuralInduction[VeritasConstruct, VeritasFormula]])

    for (obl <- noindobls) {
      val goalname = extractGoalName(obl.goal)
      val trans = new VeritasTransformer[TPTP](
        VeritasTransformerBestStrat.config, x => x.asInstanceOf[TPTP])
      val ps = pg.appliedStep(obl).get
      val parentedges = pg.requiringSteps(obl) map (_._2)
      val assumptions = pg.requiredObls(ps) map { case (o, el) => (el, o.goal) }
      val (aspec, assms, agoal) = trans.assembleFullProblem(obl.goal, obl.spec, parentedges, assumptions)

      val module: Module = Module("ConsistencyModule", Seq(), aspec.asInstanceOf[Module].defs ++ assumptions.asInstanceOf[Module].defs)

      consistencyChecker.consistent(module) match {
        case Success(true) => println(s"$goalname: No inconsistency found.")
        case Success(false) => println(s"$goalname: Inconsistency found!!!!!!!!!!!!")
      }

    }
  }


}
