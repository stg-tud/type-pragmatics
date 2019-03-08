package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.{File, FileWriter, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.strategies.VeritasProgressPreservationTopLevelStrategy
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.StructuralInduction
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier._
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.prettyprint.SimpleToScalaSPLSpecificationPrinter

import scala.util.Success

// utility class with top-level methods for generating proof graphs for progress and preservation proofs,
// verifying them, visualizing them etc.
case class ProgressPreservationProofGraphGeneration(sourcepath: String, storepath: String) {

  // path for separate folder for collecting the inconclusive problem descriptions
  private val log_inconclusive_problems_path: String = storepath + "-inconclusive"

  private val file_inconclusive = new File(log_inconclusive_problems_path)
  if (file_inconclusive.exists()) recursivedelete(file_inconclusive)
  if (!file_inconclusive.mkdir()) sys.error(s"Could not create new folder for $log_inconclusive_problems_path.")

  // path for separate folder for collecting the proved problem descriptions
  private val log_proved_path: String = storepath + "-proved"

  private val file_proved = new File(log_proved_path)
  if (file_proved.exists()) recursivedelete(file_proved)
  if (!file_proved.mkdir()) sys.error(s"Could not create new folder for $log_proved_path.")

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


  private def makeCustomVampire(timeout: Int, logic: String) = new TPTPVampireVerifier(timeout, "4.3.0", logic)
  private def makeCustomVampireZ3(timeout: Int) = new Z3VampireVerifier(timeout)
  private def makeCustomVampireTar(timeout: Int) = new ADTVampireVerifier(timeout)



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


  private def logProblem(ps: pg.ProofStep, filepath: String): Unit = {
    val goalobl = pg.targetedObl(ps)
    val goalname = extractGoalName(goalobl.goal)

    //val goalobl_ui = ppstrat.PG_UI.getObligation(goalname)

    val trans = new VeritasTransformer[TPTP](
      VeritasTransformerBestStrat.config, x => x.asInstanceOf[TPTP])

    val transSMTLIB = new VeritasTransformer[SMTLibFormat](VeritasTransformerSMTLIBStrat.config, x => x.asInstanceOf[SMTLibFormat])

    //write pretty-printed version of problem to file (logging in SPL)
    //val (aspec, assms, agoal) = ppstrat.PG_UI.getAssembledProblem[TPTP](goalobl, trans)
    val parentedges = pg.requiringSteps(goalobl) map (_._2)
    val assumptions = pg.requiredObls(ps) map { case (o, el) => (el, o.goal) }
    val (aspec, assms, agoal) = trans.assembleFullProblem(goalobl.goal, goalobl.spec, parentedges, assumptions)


    val assembledproblem_str = s"DEFINITIONS: \n ${aspec.toPrettyString()} \n\n " +
      s"GOAL-SPECIFIC ASSUMPTIONS: \n ${assms.toPrettyString()} \n\n" +
      s"GOAL: \n ${agoal.toPrettyString()}"
    val assembledProblemFile = new File(s"$filepath/$goalname-assembledProblemSPL")
    writeToFile(assembledProblemFile, assembledproblem_str)

    //pretty-print in ScalaSPL and log
    val splfile = new File(s"$filepath/$goalname-assembledProblemScalaSPL")
    val filewriter = new FileWriter(splfile)
    val splprinter = new SimpleToScalaSPLSpecificationPrinter {
      override val printer: PrettyPrintWriter = new PrettyPrintWriter(filewriter)
    }
    val goaldefs: Seq[ModuleDef] = if (agoal.isInstanceOf[Local]) agoal.asInstanceOf[Local].defs else Seq(agoal.asInstanceOf[ModuleDef])
    val fullmodule = Module("GeneratedProblem", Seq(), aspec.asInstanceOf[Module].defs ++ assms.asInstanceOf[Module].defs ++ goaldefs)
    splprinter.print(fullmodule)

    //write translated version of problem to file
    val transformedProblem = trans.translateProblem((aspec, assms, agoal))
    val translatedProblemFile = new File(s"$filepath/$goalname-TPTP_Problem")
    writeToFile(translatedProblemFile, transformedProblem.get.toString)

    //write SMTLIB translation of problem to file
    val transformedProblemSMTLIB = transSMTLIB.translateProblem((aspec, assms, agoal))
    val translatedProblemFileSMTLIB = new File(s"$filepath/$goalname-SMTLIB_Problem")
    writeToFile(translatedProblemFile, transformedProblem.get.toString)
  }

  // print step results on console and log inconclusive problem descriptions as files
  private def printStepResult(ps: pg.ProofStep, res: pg.StepResult, log_proved: Boolean = false, log_inconclusive: Boolean = true): String =
    res.status match {
      case Finished(stat, ver) =>
        stat match {
          case Proved(ie@InductionSchemeEvidence(_, _, _)) => s"Proved (induction scheme application)."
          case Proved(ATPResultDetails(_, _, _, Some(lems), Some(t))) => {
            if (log_proved) {
              logProblem(ps, log_proved_path)
            }

            s"Proved ($t s). Used lemmas: $lems"
          }
          case Proved(_) => "Proved (unknown details)."
          case Disproved(_) => "Disproved."
          case Inconclusive(_) => {
            // write pretty printed version of assembled proof problem to a file
            if (log_inconclusive) {
              logProblem(ps, log_inconclusive_problems_path)
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

  def verifyAll(provertimeout: Int = 10, log_proved: Boolean = false, log_inconclusive: Boolean = true) = {
    val indver = new TrustInductionSchemeVerifier[VeritasConstruct, VeritasFormula]()

    //decide HERE which Vampire version is to be used for all the steps (that are not induction scheme steps)
    //relies on the appropriate binaries being place in the PATH
    //current pre-programmed options:
    // 1) makeCustomVampire(provertimeout, "tff") or makeCustomVampire(provertimeout, "fof") - Vampire 4.3.0 with a TPTP encoding of the problems, calls binary named "vampire-4.3.0" (casc mode)
    // 2) makeCustomVampireZ3(provertimeout) - Vampire 4.3.0 with an SMTLIB encoding of the problems, calls binary named "vampire-4.3.0" and appends flags "--input_syntax smtlib2"
    // 3) makeCustomVampireTar(provertimeout) - Vampire 4.1 with TAR support (and an SMTLIB encoding of the problems), calls binary named "vampire-4.1_tar" and appends flags "--input_syntax smtlib2"
    val noindver = makeCustomVampireZ3(provertimeout)

    val (indobls, noindobls) = pg.obligationDFS() partition (o => pg.appliedStep(o).get.tactic.isInstanceOf[StructuralInduction[VeritasConstruct, VeritasFormula]])


    //logging on console and potentially saving problems that cannot be proved to extra files (if log_inconclusive is true):
    for (obl <- indobls) {
      val ps = pg.appliedStep(obl).get
      val goalname = extractGoalName(obl.goal)
      println(s"$goalname: " + printStepResult(ps, pg.verifyProofStep(ps, indver, None), log_proved, log_inconclusive))
    }

    for (obl <- noindobls) {
      val ps = pg.appliedStep(obl).get
      val goalname = extractGoalName(obl.goal)
      println(s"$goalname: " + printStepResult(ps, pg.verifyProofStep(ps, noindver, None), log_proved, log_inconclusive))
    }


  }

  def checkConsistencyFor(obls: Seq[pg.Obligation], provertimeout: Int = 300, log_inconsistent: Boolean = true): Unit = {
    val ver = makeCustomVampire(provertimeout, "tff")
    val consistencyChecker = ConsistencyChecker(VeritasTransformerBestStrat.config, ver.prover)

    if (log_inconsistent) {
      for (obl <- obls) {
        val goalname = extractGoalName(obl.goal)
        val trans = new VeritasTransformer[TPTP](
          VeritasTransformerBestStrat.config, x => x.asInstanceOf[TPTP])
        val ps = pg.appliedStep(obl).get
        val parentedges = pg.requiringSteps(obl) map (_._2)
        val assumptions = pg.requiredObls(ps) map { case (o, el) => (el, o.goal) }
        val (aspec, assms, agoal) = trans.assembleFullProblem(obl.goal, obl.spec, parentedges, assumptions)
        val assembledproblem_str = s"DEFINITIONS: \n ${aspec.toPrettyString()} \n\n " +
          s"GOAL-SPECIFIC ASSUMPTIONS: \n ${assms.toPrettyString()} \n\n" +
          s"GOAL: \n ${agoal.toPrettyString()}"
        val assembledProblemFile = new File(s"ConsistencyCheck/$goalname")
        writeToFile(assembledProblemFile, assembledproblem_str)
        val module: Module = Module("ConsistencyModule", Seq(), aspec.asInstanceOf[Module].defs ++ assms.asInstanceOf[Module].defs)

        consistencyChecker.consistent(module) match {
          case Success(true) => println(s"$goalname: No inconsistency found.")
          case Success(false) => println(s"$goalname: Inconsistency found!!!!!!!!!!!!")
        }

      }
    }
  }



  def checkConsistencyAll(log_inconsistent: Boolean = true, provertimeout: Int = 300): Unit = {
    //deliberately include top-level theorems with induction steps. Vampire may find counter-examples pretty well
    //if a problem does NOT hold
    val allobls = pg.obligationDFS()
    checkConsistencyFor(allobls, provertimeout, log_inconsistent)
  }

  def checkConsistencyTopLevelProblems() = {
    val progressobl = pg.findObligation("Progress")
    val preservationobl = pg.findObligation("Preservation")

    for (o <- progressobl) checkConsistencyFor(Seq(o))
    for (o <- preservationobl) checkConsistencyFor(Seq(o))
  }


}
