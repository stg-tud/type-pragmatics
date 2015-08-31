package veritas.benchmarking

import java.io.File

import scopt.OptionParser
import veritas.benchmarking.vampire.{VampireTraceAnalisisOptions, VampireTraceAnalisis, VampireConfig}

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
   * @return human-readable summary of details as string
   */
  def toHumanString: String
}
case class StringDetails(details: String) extends ResultDetails {
  override def toString = details
  override def toHumanString = details
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

  def findBinaryInPath(command: String): File = {
    for (p <- System.getenv("PATH").split(File.pathSeparator);
         f = new File(p, command) if f.exists() && f.canExecute)
      return f
    null
  }

  def makeCall(file: File, timeout: Int): Seq[String]
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
    _configs += c.name -> c
  }
  _contributedOptions = _contributedOptions :+ VampireTraceAnalisisOptions

  _configs filter (kv => if (kv._2.isValid) true else {println(s"** Removing invalid configuration ${kv._1}"); false})

  def configs = _configs
  def contributedOptions = _contributedOptions
}



