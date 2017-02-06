package veritas.benchmarking

import java.io._

import scopt.OptionParser
import veritas.benchmarking.beagle.BeagleConfig
import veritas.benchmarking.vampire.{VampireConfig, VampireTraceAnalisis, VampireTraceAnalisisOptions}
import veritas.benchmarking.princess.{PrincessCascConfig, PrincessCascSlurmConfig, PrincessStandardConfig, PrincessStandardSlurmConfig}
import veritas.benchmarking.eprover.EproverConfig

import scala.sys.process.{FileProcessLogger, ProcessLogger}

sealed trait ProverStatus

case object Proved extends ProverStatus

case object Disproved extends ProverStatus

case class Inconclusive(terminationReason: String) extends ProverStatus

trait ResultDetails {
  /**
    * @return all details as string
    */
  def toString: String

  /**
    *
    * @return details as list (e.g. used lemmas)
    */
  def toList: List[String] = List()

  /**
    * @return human-readable summary of details as string
    */
  def toHumanString: String
}

case class StringDetails(details: String, lemmas: List[String] = List()) extends ResultDetails {
  override def toString = details

  override def toList = lemmas

  override def toHumanString = if (lemmas.isEmpty) details
  else
    s"Used Lemmas: ${lemmas.length} ${lemmas.mkString(" - ", ", ", "")}"
}

class ProverResult(
                    val status: ProverStatus,
                    val timeSeconds: Option[Double],
                    val details: ResultDetails
                  )

trait ProverConfig {
  def isValid: Boolean

  val name: String
  val proverCommand: File
  val acceptedFileFormats: Set[String]
  val modulesToLoad: Set[String] = Set()

  def createProverCallHHlr(proverpath: String, provercall: Seq[String]) =
    proverpath + provercall(0).split("/").last + " " + provercall.drop(1).mkString(" ")

  // todo: automatically append .exe under windows systems
  def findBinaryInPath(command: String): File = {
    for (p <- System.getenv("PATH").split(File.pathSeparator);
         f = new File(p, command) if f.exists() && f.canExecute)
      return f
    null
  }

  def findFileInPath(command: String): File = {
    for (p <- System.getenv("PATH").split(File.pathSeparator);
         f = new File(p, command) if f.exists())
      return f
    null
  }

  def makeCall(file: File, timeout: Int, fullLogs: Boolean): Seq[String]

  def newResultProcessor(outfile: File, defaultTimeout: Int, processLogsOnly: Boolean = false): ResultProcessor
}

abstract class ResultProcessor(outfile: File, defaultTimeout: Int, processLogsOnly: Boolean = false) extends ProcessLogger {
  def result: ProverResult

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

trait ContributedOptions {
  def contributeOptions(p: OptionParser[Main.Config]): Unit
}

object ProverConfig {
  private var _configs: Map[String, ProverConfig] = Map()
  private var _contributedOptions = Seq[ContributedOptions]()

  for (version <- Seq("3.0", "4.0")) {
    val c = VampireConfig(version)
    val c_sat = VampireConfig(version, "vampire-sat", "casc_sat")
    _configs += c.name -> c
    _configs += c_sat.name -> c_sat
  }

  _configs += "princess" -> new PrincessCascConfig()
  _configs += "princessSlurm" -> PrincessCascSlurmConfig()
  _configs += "princessSlurm-standard" -> PrincessStandardSlurmConfig()
  _configs += "princess-standard" -> new PrincessStandardConfig
  _configs += "eprover" -> EproverConfig()
  //_configs += "beagle" -> BeagleConfig() //doesn't work yet

  _contributedOptions = _contributedOptions :+ VampireTraceAnalisisOptions

  _configs filter (kv => if (kv._2.isValid) true
  else {
    println(s"** Removing invalid configuration ${kv._1}");
    false
  })

  def configs = _configs

  def contributedOptions = _contributedOptions
}



