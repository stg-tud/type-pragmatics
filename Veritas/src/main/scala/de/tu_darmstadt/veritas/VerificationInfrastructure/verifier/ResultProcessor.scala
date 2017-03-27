package de.tu_darmstadt.veritas.VerificationInfrastructure.verifier

import java.io._

import scala.sys.process.ProcessLogger


/**
  * abstract class for processing the result produced by a prover
  * from benchmarking project
  *
  * @param outfile
  * @param defaultTimeout
  * @param processLogsOnly
  */
abstract class ResultProcessor(outfile: File, defaultTimeout: Double, processLogsOnly: Boolean = false) extends ProcessLogger {
  def result: ProverStatus

  //use own writer to make sure that old log files are overwritten (FileProcessLogger uses writer with append = true)
  val writer = new PrintWriter(
    new BufferedWriter(
      new OutputStreamWriter(
        new FileOutputStream(outfile, processLogsOnly))))

  //reads the outfile of this ResultProcessor and processes them using extractProverResult
  def processLogs() = {
    val fileLines = io.Source.fromFile(outfile).getLines
    for (line <- fileLines)
      extractProverResult(line)
  }

  //override to define how a result processor processes the output of a prover
  def extractProverResult(s: => String): Unit

  //log output of prover to a separate file (given by outfile)
  override def out(s: => String) = {
    writer println s
    flush()
  }

  //log output of prover to a separate file (given by outfile)
  override def err(s: => String) = {
    writer println s
    flush()
  }

  def close(): Unit =
    writer.close()

  def flush(): Unit =
    writer.flush()
}