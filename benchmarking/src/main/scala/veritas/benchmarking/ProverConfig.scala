package veritas.benchmarking

import java.io.File

import veritas.benchmarking.vampire.VampireConfig

sealed trait ProverResult {
  def status: String
  def details: String
}
case class Proved(proof: String) extends ProverResult {
  def status = "proved"
  def details = proof
}
case class Disproved(disproof: String) extends ProverResult {
  def status = "disproved"
  def details = disproof
}
case class Inconclusive(hint: String) extends ProverResult {
  def status = "inconclusive"
  def details = hint
}

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

  def analyzeOutput(output: String): ProverResult
  def tryExtractTimeSeconds(output: String): Option[Double]
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



