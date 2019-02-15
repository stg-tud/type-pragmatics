package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.strategies.VeritasProgressPreservationTopLevelStrategy
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.StructuralInduction
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier._
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.util.FreshNames

object ProgressPreservationGraphGeneration extends App {


  val aesource = "src/test/scala/de/tu_darmstadt/veritas/scalaspl/AESpec.scala"
  val aestore = "AESoundnessGeneratedProofGraph-store"

  val ppstrat = new VeritasProgressPreservationTopLevelStrategy(aesource, aestore)

  val pg_gen = ppstrat.generateFullGraph()

  //ppstrat.printAllObligations()

  val pg = ppstrat.PG_UI.pg

  // create separate folder for collecting the inconclusive problem descriptions
  val file_inconclusive = new File("AESoundnessProof-allsteps-inconclusive")
  if (file_inconclusive.exists()) recursivedelete(file_inconclusive)
  if (!file_inconclusive.mkdir()) sys.error("Could not create new folder for AESoundnessProof-allsteps-inconclusive.")

  val fresh = new FreshNames()

  val (indobls, noindobls) = pg.obligationDFS() partition (o => pg.appliedStep(o).get.tactic.isInstanceOf[StructuralInduction[VeritasConstruct, VeritasFormula]])

  val indver = new TrustInductionSchemeVerifier[VeritasConstruct, VeritasFormula]()
  //use Vampire (4.1) with 120 sec timeout and tff encoding for verifying all steps that are not induction scheme applications
  val noindver = makeCustomVampire(10, "tff")

  //logging on console and saving problems that cannot be proved to extra files:
  for (obl <- indobls) {
    val ps = pg.appliedStep(obl).get
    val goalname = extractGoalName(obl.goal)
    println(s"$goalname: " + printStepResult(ps, pg.verifyProofStep(ps, indver, None)))
  }

  for (obl <- noindobls) {
    val ps = pg.appliedStep(obl).get
    val goalname = extractGoalName(obl.goal)
    println(s"$goalname: " + printStepResult(ps, pg.verifyProofStep(ps, noindver, None)))
  }

  ppstrat.visualizeGraph("GeneratedAESoundness.png")



  //helper functions (put in a utility class?)
  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete)
    file.delete
  }

  def makeCustomVampire(timeout: Int, logic: String) = new TPTPVampireVerifier(timeout, "4.1", logic)


  def extractGoalName(vc: VeritasConstruct): String =
    vc match {
      case Goals(gl, _) => gl.head.name
      case Lemmas(ll, _) => ll.head.name
      case Axioms(axl) => axl.head.name
      case TypingRule(name, _, _) => name
      case _ => fresh.freshRuleName("obl_")
    }

  def writeToFile(filehandler: File, s: String) = {
    if (!filehandler.getParentFile.exists())
      filehandler.getParentFile.mkdirs()
    filehandler.createNewFile()
    new PrintWriter(filehandler) {
      write(s);
      close
    }
  }


  // print step results on console and log inconclusive problem descriptions as files
  def printStepResult(ps: pg.ProofStep, res: pg.StepResult): String =
    res.status match {
      case Finished(stat, ver) =>
        stat match {
          case Proved(ie@InductionSchemeEvidence(_, _, _)) => s"Proved (induction scheme application)."
          case Proved(ATPResultDetails(_, _, _, _, Some(t))) => "Proved (" + t + "s)."
          case Proved(_) => "Proved (unknown details)."
          case Disproved(_) => "Disproved."
          case Inconclusive(_) => {
            // write pretty printed version of assembled proof problem to a file
            val goalobl = pg.targetedObl(ps)
            val goalname = extractGoalName(goalobl.goal)

//            val goalobl_ui = ppstrat.PG_UI.getObligation(goalname)
//
//            val trans = new VeritasTransformer[TPTP](
//              Configuration(Map(FinalEncoding -> FinalEncoding.TFF,
//                Simplification -> Simplification.LogicalAndConstructors,
//                VariableEncoding -> VariableEncoding.InlineEverything,
//                Selection -> Selection.SelectAll,
//                Problem -> Problem.All)), x => x.asInstanceOf[TPTP])
//
//            //write pretty-printed version of problem to file
//            val (aspec, assms, agoal) = ppstrat.PG_UI.getAssembledProblem[TPTP](goalobl_ui, trans)
//            val assembledproblem_str = s"DEFINITIONS: \n ${aspec.toPrettyString()} \n\n " +
//              s"GOAL-SPECIFIC ASSUMPTIONS: \n ${assms.toPrettyString()} \n\n" +
//              s"GOAL: \n ${agoal.toPrettyString()}"
//            val assembledProblemFile = new File(s"AESoundnessProof-allsteps-inconclusive/$goalname-assembledProblem")
//            writeToFile(assembledProblemFile, assembledproblem_str)
//
//            //write translated version of problem to file
//            val transformedProblem = trans.translateProblem((aspec, assms, agoal))
//            val translatedProblemFile = new File(s"AESoundnessProof-allsteps-inconclusive/$goalname-translatedProblem")
//            writeToFile(translatedProblemFile, transformedProblem.get.toString)

            "Inconclusive."
          }
          case ProverFailure(_) => "Failure."
        }
      case Unknown(_) => "Unknown"
      case VerifierFailure(err, _) => "VerifierFailure :" + err
    }


  //ppstrat.printGoalWithName("Preservation")

  //  val qlsource = "src/test/scala/de/tu_darmstadt/veritas/scalaspl/QLSpec.scala"
  //  val qlstore =  "QLSoundnessGeneratedProofGraph-store"
  //
  //  val ppstrat_ql = new ProgressPreservationTopLevelStrategy(qlsource, qlstore)
  //
  //  val pg_gen_gl = ppstrat_ql.generateGraph()
  //
  //  ppstrat_ql.visualizeGraph("GeneratedQLSoundness.png")
  //
  //  ppstrat_ql.printAllObligations()

}
