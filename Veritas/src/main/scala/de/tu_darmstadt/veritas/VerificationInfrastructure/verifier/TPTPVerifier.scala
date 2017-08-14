package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast._

import scala.util.{Failure, Success}

trait TPTPVerifier extends Verifier[VeritasConstruct, VeritasConstruct] {
  override type V = TPTP

  def prover: Prover[TPTP]

  def transformer = new VeritasTransformer[TPTP](
    Configuration(Map(FinalEncoding -> FinalEncoding.TFF,
      Simplification -> Simplification.LogicalAndConstructors,
      VariableEncoding -> VariableEncoding.InlineEverything,
      Selection -> Selection.SelectAll,
      Problem -> Problem.All)), x => x.asInstanceOf[TPTP])

  protected def extractGoalName(vc: VeritasConstruct): String =
    vc match {
      case Goals(gl, _) => gl.head.name
      case Lemmas(ll, _) => ll.head.name
      case Axioms(axl) => axl.head.name
      case TypingRule(name, _, _) => name
      case _ => "goal" //should not happen
    }

  protected def writeToFile(filehandler: File, s: String): Unit = {
    if (!filehandler.getParentFile.exists())
      filehandler.getParentFile.mkdirs()
    filehandler.createNewFile()
    new PrintWriter(filehandler) {
      write(s)
      close()
    }
  }

  override def verify[Result <: GenStepResult[VeritasConstruct, VeritasConstruct]]
  (goal: VeritasConstruct,
   spec: VeritasConstruct,
   parentedges: Iterable[EdgeLabel],
   assumptions: Iterable[VeritasConstruct],
   hints: Option[VerifierHints],
   produce: StepResultProducer[VeritasConstruct, VeritasConstruct, Result],
   pathforlogs: Option[String] = None): Result = {
    spec match {
      case Module(name, imps, moddefs) => {
        val transformedProb = transformer.transformProblem(goal, spec, parentedges, assumptions)
        val fileending =
          if (transformer.config.contains(Configuration(Map(FinalEncoding -> FinalEncoding.TFF))))
            ".tff"
          else ".fof"
        val goalname = extractGoalName(goal)

        transformedProb match {
          case Success(tptp) => {
            //for debugging purposes
            //println("TPTP file: ")
            //println(tptp.toString)

            val proverstatus = prover.callProver(tptp)

            val res = produce.newStepResult(Finished(proverstatus, this),
              proverstatus.proverResult.proofEvidence,
              proverstatus.proverResult.message)

            if (pathforlogs.isDefined) {
              val tptpfile = new File(pathforlogs.get + extractGoalName(goal) + fileending)
              val evidencefile = new File(pathforlogs.get + goalname + ".log")
              writeToFile(tptpfile, tptp.toString)
              writeToFile(evidencefile, proverstatus.proverResult.fullLogs)
            }

            res

          }
          case Failure(err) => {
            produce.newStepResult(VerifierFailure(s"Problem during transformation step: $err", this), None, None)
          }
        }
      }
      case _ => produce.newStepResult(VerifierFailure(s"Specification was not a module $spec", this), None, None)
    }
  }
}
