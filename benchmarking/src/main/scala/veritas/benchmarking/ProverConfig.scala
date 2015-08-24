package veritas.benchmarking

import java.io.File

import veritas.benchmarking.vampire.VampireConfig

import scala.sys.process.ProcessLogger

sealed trait ProverStatus
case object Proved extends ProverStatus
case object Disproved extends ProverStatus
case class Inconclusive(terminationReason: String) extends ProverStatus

class ProverResult(
  val status: ProverStatus,
  val timeSeconds: Option[Double],
  val details: Any
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

object ProverConfig {
  private var _configs: Map[String, ProverConfig] = Map()

  for (version <- Seq("3.0", "4.0")) {
    val c = VampireConfig(version)
    _configs += c.name -> c
  }

  _configs filter (kv => if (kv._2.isValid) true else {println(s"** Removing invalid configuration ${kv._1}"); false})

  def configs = _configs
}



