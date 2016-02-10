package veritas.benchmarking

import java.io.File

import scopt.OptionParser
import veritas.benchmarking.beagle.BeagleConfig
import veritas.benchmarking.vampire.{VampireTraceAnalisisOptions, VampireTraceAnalisis, VampireConfig}
import veritas.benchmarking.princess.{NewPrincessConfig, PrincessConfig}
import veritas.benchmarking.eprover.EproverConfig

import scala.sys.process.ProcessLogger

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
  override def toHumanString = if (lemmas.isEmpty) details else
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

  // todo: automatically append .exe under windows systems
  def findBinaryInPath(command: String): File = {
    for (p <- System.getenv("PATH").split(File.pathSeparator);
         f = new File(p, command) if f.exists() && f.canExecute)
      return f
    null
  }

  def findFileInPath(command: String) : File = {
    for (p <- System.getenv("PATH").split(File.pathSeparator);
         f = new File(p, command) if f.exists())
      return f
    null
  }

  def makeCall(file: File, timeout: Int, fullLogs: Boolean): Seq[String]
  def newResultProcessor(file: File, timeout: Int): ResultProcessor
}

trait ResultProcessor extends ProcessLogger {
  def result: ProverResult
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

  _configs += "princess-casc" -> PrincessConfig()
  _configs += "princess-standard" -> NewPrincessConfig()
  _configs += "eprover" -> EproverConfig()
  //_configs += "eprover" -> BeagleConfig() //doesn't work yet

  _contributedOptions = _contributedOptions :+ VampireTraceAnalisisOptions

  _configs filter (kv => if (kv._2.isValid) true else {println(s"** Removing invalid configuration ${kv._1}"); false})

  def configs = _configs
  def contributedOptions = _contributedOptions
}



