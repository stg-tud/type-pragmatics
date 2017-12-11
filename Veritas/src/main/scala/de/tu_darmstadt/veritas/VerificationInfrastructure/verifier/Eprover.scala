package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import java.io.{File, PrintWriter}

/**
  * Expects the correct Eprover binaries in the project path, named "eprover"
  */
case class Eprover(timeout: Int) extends Prover[TPTP] {
  private val proverCommand =
    findBinaryInPath("eprover")

  private def makeCall(file: File, timeout: Int) = {
    var call = Seq(proverCommand.getAbsolutePath)
    call = call ++ Seq("--auto", "--tptp3-format", "--resources-info", "--proof-object")
    if (timeout > 0)
      call = call :+ ("--cpu-limit=" + timeout.toString)

    call = call :+ file.getAbsolutePath
    call
  }

  override def callProver(problem: TPTP): ProverStatus = {
    import scala.sys.process._

    val start = System.nanoTime()
    val suffix = problem match {
      case TFFFormat(_) => "tff"
      case FOFFormat(_) => "fof"
    }
    //make temporary files for prover input and output
    val tempInFile = File.createTempFile("problem-to-prove", suffix)
    val tempOutFile = File.createTempFile("proveroutput", "out")
    writeToFile(tempInFile, problem.toString)

    val resultProc = EproverResultProcessor(tempOutFile, timeout)


    val proverCall = makeCall(tempInFile, timeout)
    val p = proverCall.run(resultProc)

    import concurrent._
    import ExecutionContext.Implicits.global
    val f = Future(blocking(p.exitValue()))

    try {
      val code = Await.result(f, duration.Duration(timeout + 10, "sec"))
      val end = System.nanoTime()

      //process logs once at the end
      resultProc.processLogs()

      resultProc.result

    } catch {
      case _: TimeoutException =>
        p.destroy()
        println(s" *** Timeout when calling ${proverCall.head}...")
        resultProc.result
    }

  }
}

case class EproverResultProcessor(outfile: File, defaultTimeout: Double, processLogsOnly: Boolean = false)
  extends ResultProcessor(outfile, defaultTimeout, processLogsOnly) {

  var proved: Option[Boolean] = None
  var error: Boolean = false
  var time: Option[Double] = Some(defaultTimeout) // default value: timeout for prover (since eprover does not report a time if it was unsuccessful)
  var logBuilder: StringBuilder = StringBuilder.newBuilder
  var proofBuilder: StringBuilder = _
  // TODO: can we extract proofs?
  var proof: String = ""
  var message: String = ""
  var lemmas: List[String] = List()

  var proofOutputRunning = false

  override def extractProverResult(s: => String): Unit = {
    try {
      logBuilder ++= s
      if (s.contains("# SZS status Theorem"))
        proved = Some(true)
      else if (s.contains("# SZS status CounterSatisfiable")) {
        proved = Some(false)
      } else if (s.contains("# Failure:")) {
        proved = None
        message = s.substring(s.indexOf("# Failure:") + "# Failure".length)
      } else if (s.contains("# SZS output start CNFRefutation.")) {
        proofOutputRunning = true
        proofBuilder = StringBuilder.newBuilder
      } else if (s.contains("# SZS output end CNFRefutation.")) {
        proof = proofBuilder.toString
        proofBuilder = null
        proofOutputRunning = false

      } else if (s.contains("Total time")) {
        time = tryExtractTimeSeconds(s)
      }

      if (proofOutputRunning && !s.startsWith("#")) {
        proofBuilder ++= s

        val lemmaregex = """file\('.+',(.+)\)\)""".r.unanchored
        s match {
          case lemmaregex(lemmaname) => lemmas = List(lemmaname) ++ lemmas
          case _ =>
        }
      }
    } catch {
      case e: Exception => println(s"Error ${e.getMessage} in $s")
        error = true
        message = s"Error ${e.getMessage} in $s"
        throw e
    }
  }

  private def tryExtractTimeSeconds(output: String) = {
    val begin = output.indexOf(":") + 1
    val end = output.indexOf("s", begin) - 1
    if (begin >= 0 && end > 0)
      Some(output.substring(begin, end).toDouble)
    else
      None
  }

  override def buffer[T](f: => T) = f
}


