package veritas.benchmarking

import java.io.File

sealed trait ProverResult
case class Proved(proof: String) extends ProverResult
case class Disproved(disproof: String) extends ProverResult
case class Inconclusive(hint: String) extends ProverResult

trait ProverConfig {
  def isValid: Boolean

  val name: String
  val proverCommand: File
  val timeout: Int

  def findBinaryInPath(command: String): File = {
    for (p <- System.getenv("PATH").split(File.pathSeparator);
         f = new File(p, command) if f.exists() && f.canExecute)
      return f
    null
  }

  def makeCall(file: File): Seq[String]

  def analyzeOutput(output: String): ProverResult
  def tryExtractTimeSeconds(output: String): Option[Double]
}

object ProverConfig {
  private var _configs: Map[String, ProverConfig] = Map()

  for (version <- Seq("3.0", "4.0");
       timeout <- Seq(10, 30, 60, 300)) {
    val c = VampireConfig(version, timeout)
    _configs += c.name -> c
  }

  _configs filter (kv => if (kv._2.isValid) true else {println(s"** Removing invalid configuration ${kv._1}"); false})

  def configs = _configs
}

case class VampireConfig(version: String, timeout: Int, mode: String = "casc") extends ProverConfig {
  def isValid = proverCommand != null

  override val name = if (version == null) s"vampire-t$timeout" else s"vampire-$version-t$timeout"
  override val proverCommand = findBinaryInPath(if (version == null) s"vampire" else s"vampire-$version")

  def makeCall(file: File) = {
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

  override def analyzeOutput(output: String) = {
    if (output.contains("Termination reason: Refutation")) {
      // proved
      val start = output.indexOf("start Proof")
      val startline = output.indexOf('\n', start)
      val end = output.indexOf("end Proof")
      val endline = output.lastIndexOf('\n', end)
      val proof = output.substring(startline, endline)
      Proved(proof)
    }
    else if (output.contains("Termination reason: Satisfiable")) {
      Disproved("no counter example specified")
    }
    else {
      val start = output.indexOf("Termination reason: ") + "Termination reason: ".length
      val lineEnd = output.indexOf('\n', start)
      Inconclusive(output.substring(start, lineEnd))
    }
  }

  def tryExtractTimeSeconds(output: String) = {
    val start = output.indexOf(" in time ")
    val end = output.indexOf(" s", start)
    if (start < 0 || end < 0)
      None
    else {
      val s = output.substring(start + " in time ".length, end)
      val d = s.toDouble
      Some(d)
    }
  }
}
