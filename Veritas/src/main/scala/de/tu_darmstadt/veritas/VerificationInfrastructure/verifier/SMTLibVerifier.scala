package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.Configuration._
import de.tu_darmstadt.veritas.backend.ast._

import scala.util.{Failure, Success}

/**
  * Created by andiderp on 10.07.17.
  */
trait SMTLibVerifier extends Verifier[VeritasConstruct, VeritasFormula] {
  override type V = SMTLibFormat

  def prover: Prover[SMTLibFormat]

  protected def extractGoalName(vc: VeritasFormula): String =
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

  override def verify[Result <: GenStepResult[VeritasConstruct, VeritasFormula]]
  (goal: VeritasFormula,
   spec: VeritasConstruct,
   parentedges: Iterable[EdgeLabel],
   assumptions: Iterable[VeritasFormula],
   hints: Option[VerifierHints],
   produce: StepResultProducer[VeritasConstruct, VeritasFormula, Result],
   pathforlogs: Option[String] = None): Result = {
    val transformer = new VeritasTransformer[SMTLibFormat](
      Configuration(Map(FinalEncoding -> FinalEncoding.SMTLib,
        Simplification -> Simplification.LogicalAndConstructors,
        VariableEncoding -> VariableEncoding.InlineEverything,
        Selection -> Selection.SelectAll,
        Problem -> Problem.All)), x => x.asInstanceOf[SMTLibFormat])
    spec match {
      case Module(name, imps, moddefs) => {
        val transformedProb = transformer.transformProblem(goal, spec, parentedges, assumptions)
        val fileending = ".smt2"
        val goalname = extractGoalName(goal)

        transformedProb match {
          case Success(smtLib) => {
            //for debugging purposes
            //println("SMTLIB file: ")
            //println(smtLib.toString)

            val proverstatus = prover.callProver(smtLib)

            val res = produce.newStepResult(Finished(proverstatus, this),
              proverstatus.proverResult.proofEvidence,
              proverstatus.proverResult.message)

            if (pathforlogs.isDefined) {
              val smtfile = new File(pathforlogs.get + goalname + fileending)
              val evidencefile = new File(pathforlogs.get + goalname + ".log")
              writeToFile(smtfile, smtLib.toString)
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
