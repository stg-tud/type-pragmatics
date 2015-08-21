package veritas.benchmarking.vampire

import java.io.File

import veritas.benchmarking._

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

  def makeSatCall(file: File, timeout: Int) = {
    var call = Seq(proverCommand.getAbsolutePath)

    if (timeout > 0)
      call = call ++ Seq("-t", timeout.toString)

    call = call ++ Seq("--saturation_algorithm", "fmb")

    call = call ++ Seq("--proof", "tptp")
    call = call ++ Seq("--output_axiom_names", "on")

    call = call :+ file.getAbsolutePath
    call
  }

  override def analyzeOutput(output: String, file: File, timeout: Int) = {
    if (output.contains("Termination reason: Refutation\n")) {
      // proved
      val start = output.indexOf("start Proof")
      val startline = output.indexOf('\n', start) + 1
      val end = output.indexOf("end Proof")
      val endline = output.lastIndexOf('\n', end)
      val proof = output.substring(startline, endline)
      Proved(proof)
    }
    else if (output.contains("Termination reason: Satisfiable")) {
      val call = makeSatCall(file, timeout)
      val (satout, _) = Runner.exec(call, false)

      val start = satout.indexOf("Satisfiable!\n")
      val startline = start + "Satisfiable!\n".length
      val end = satout.indexOf("-------")

      if (start >= 0 && end >= 0) {
        val model = satout.substring(startline, end).trim
        Disproved(model)
      }
      else
        Disproved("no counter example found")
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
