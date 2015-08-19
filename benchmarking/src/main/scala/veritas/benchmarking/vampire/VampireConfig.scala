package veritas.benchmarking.vampire

import java.io.File

import veritas.benchmarking.{Disproved, Inconclusive, Proved}

case class VampireConfig(version: String, mode: String = "casc") extends ProverConfig {
  def isValid = proverCommand != null

  override val name = if (version == null) s"vampire" else s"vampire-$version"
  override val proverCommand = findBinaryInPath(name)

  def makeCall(file: File, timeout: Int) = {
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
    if (output.contains("Termination reason: Refutation\n")) {
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
