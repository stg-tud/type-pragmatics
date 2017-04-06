package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.{Evidence, TSTP}

/**
  * Expects the correct Vampire binaries in the project path, named "vampire-3.0", "vampire-4.0" or "vampire-4.1"
  *
  * @param timeout in seconds
  */
case class Vampire(version: String, timeout: Int, mode: String = "casc") extends Prover[TPTP] {
  private val proverCommand =
    findBinaryInPath(if (version == null) s"vampire" else s"vampire-$version")

  private def makeCall(file: File, timeout: Int) = {
    var call = Seq(proverCommand.getAbsolutePath)

    if (timeout > 0)
      call = call ++ Seq("-t", timeout.toString)

    if (mode != null)
      call = call ++ Seq("--mode", mode)

    call = call ++ Seq("--proof", "tptp")
    call = call ++ Seq("--output_axiom_names", "on")

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

    val resultProc = VampireResultProcessor(tempOutFile, timeout)


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

case class TSTPProof(proof: String) extends TSTP[String] {
  override def getData: String = proof

  override def compare(that: Evidence): Int = this.hashCode() compare that.hashCode()
}


case class ATPResultDetails(logs: String,
                            message: Option[String],
                            proof: Option[String] = None,
                            usedLemmas: Option[List[String]] = None,
                            usedtime: Option[Double] = None) extends ResultDetails {
  /**
    *
    * @return full logs of prover
    */
  override def fullLogs: String = logs

  override def summaryDetails: String = s"Message: ${message.getOrElse("No message.")} \n\n" +
    s"used axioms/lemmas: ${usedLemmas.getOrElse(List("No data about used axioms/lemmas")).mkString(", ")} \n\n" +
    s"found in time: ${usedtime.getOrElse("No time information")} \n\n" +
    s"Proof: ${proof.getOrElse("No proof available")}"


  override def proofEvidence: Option[Evidence] = proof map (p => TSTPProof(p))

}


case class VampireResultProcessor(outfile: File, defaultTimeout: Double, processLogsOnly: Boolean = false)
  extends ResultProcessor(outfile, defaultTimeout, processLogsOnly) {

  var proved: Option[Boolean] = None //None: Inconclusive status!
  var error: Boolean = true
  var time: Option[Double] = Some(defaultTimeout)

  var logBuilder: StringBuilder = StringBuilder.newBuilder
  var proofBuilder: StringBuilder = _
  var proof: String = _
  var message: String = _
  var lemmas: List[String] = List()


  override def extractProverResult(s: => String): Unit = {
    try {
      //first, append any line found to the logs
      logBuilder ++= s
      // parse status
      if (s.endsWith("Termination reason: Refutation"))
        proved = Some(true)
      else if (s.contains("Termination reason: Satisfiable")) {
        proved = Some(false)
      }
      else if (s.contains("Termination reason: ")) {
        val start = s.indexOf("Termination reason: ") + "Termination reason: ".length
        error = false
        message = s.substring(start)
      }

      // parse proof
      else if (s.contains("start Proof")) {
        proofBuilder = StringBuilder.newBuilder
        proof = null
      }
      else if (s.contains("end Proof")) {
        proof = proofBuilder.toString
        proofBuilder = null
      }
      else if (proofBuilder != null) {
        proofBuilder ++= s
        val lemmaregex = """file\('.+',(.+)\)\)""".r.unanchored
        s match {
          case lemmaregex(lemmaname) => lemmas = List(lemmaname) ++ lemmas
          case _ =>
        }
      }

      // parse time
      else if (s.contains(" in time ")) {
        val start = s.indexOf(" in time ")
        val end = s.indexOf(" s", start)
        if (start < 0 || end < 0)
          time = None
        else {
          val timeS = s.substring(start + " in time ".length, end)
          val timeD = timeS.toDouble
          time = Some(timeD)
        }
      }

    } catch {
      case e: Exception => {
        error = true
        message = s"Error ${e.getMessage} in $s"
        throw e //throw on so that caller can decide on further steps
      }
    }
  }

  override def buffer[T](f: => T) = f
}
