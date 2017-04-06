package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import java.io.File

/**
  * Expects the correct Princess binaries in the project path, named "princess-all-casc.jar"
  *
  * @param timeout in seconds
  */
case class Princess(timeout: Int) extends Prover[TPTP] {
  private val javaCommand = findBinaryInPath(s"java")
  private val jar = findBinaryInPath(s"princess-all-casc.jar")

  def makeCall(file: File, timeout: Int) = {
    var call = Seq(javaCommand.getAbsolutePath)
    call = call ++ Seq("-Xss20000k", "-Xmx1500m", "-noverify", "-cp", jar.getAbsolutePath, "ap.CmdlMain", "-inputFormat=tptp")
    if (timeout > 0)
      call = call :+ ("-timeout=" + timeout.toString)

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

    val resultProc = PrincessResultProcessor(tempOutFile, timeout)


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


case class PrincessResultProcessor(outfile: File, defaultTimeout: Double, processLogsOnly: Boolean = false)
  extends ResultProcessor(outfile, defaultTimeout, processLogsOnly) {

  var proved: Option[Boolean] = None
  var error: Boolean = false
  var time: Option[Double] = Some(defaultTimeout)

  var logBuilder: StringBuilder = StringBuilder.newBuilder
  var proofBuilder: StringBuilder = _
  // TODO: how to extract proof?
  var proof: String = ""
  var message: String = ""
  var lemmas: List[String] = List()

  override def extractProverResult(s: => String) = {
    try {
      logBuilder ++= s
      if (s.contains("% SZS status Timeout"))
        proved = None
      else if (s.contains("% SZS status Theorem")) {
        proved = Some(true)
      }
      else if (proved.getOrElse(false) == true) {
        if (s.startsWith("{")) {
          lemmas = s.substring(1, s.indexOf("}")).split(",").toList
        }
      }
      else if (s.contains("ms")) {
        time = tryExtractTimeSeconds(s)
      }
    } catch {
      case e: Exception => println(s"Error ${e.getMessage} in $s")
        error = true
        message = s"Error ${e.getMessage} in $s"
        throw e
    }
  }

  private def tryExtractTimeSeconds(output: String): Option[Double] = {
    val timeregex = """([0-9]+)ms""".r.unanchored
    val provedregex = s""".+:.+proved.+\\(${timeregex.regex}\\)""".r.unanchored

    //try to match "Prover 0: proved (xxxxms)" first, then just xxxxms (from end of file)
    output match {
      case provedregex(time) => Some(time.toDouble / 1000)
      case timeregex(time) => Some(time.toDouble / 1000)
      case _ => None
    }
  }

  override def buffer[T](f: => T) = f

  // no setup or teardown
  override def err(s: => String) = try {
    super.err(s)
    if (s.contains("ms")) {
      time = tryExtractTimeSeconds(s)
    }
  } catch {
    case e: Exception => println(s"Error ${e.getMessage} in $s")
      throw e
  }
}
