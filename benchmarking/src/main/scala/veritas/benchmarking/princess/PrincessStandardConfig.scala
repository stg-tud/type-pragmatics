package veritas.benchmarking.princess

import java.io.File
import java.util.regex.Pattern

import veritas.benchmarking._

case class PrincessStandardConfig()
  extends ProverConfig {

  def isValid = proverCommand != null

  override val name = s"princess-standard"
  override val proverCommand = findBinaryInPath(s"java")

  override val acceptedFileFormats = Set(".fof")

  def makeCall(file: File, timeout: Int, fullLogs: Boolean) = {
    var call = Seq(proverCommand.getAbsolutePath)
    call = call ++ Seq("-Xss20000k", "-Xmx1500m", "-noverify", "-cp", "princess-all-standard.jar", "ap.CmdlMain", "-inputFormat=tptp")
    if (timeout > 0)
    // accepts timeout in ms
      call = call :+ ("-timeout=" + (timeout * 1000).toString)


    call = call :+ "+unsatCore"
    call = call :+ file.getAbsolutePath
    call
  }

  def tryExtractTimeSeconds(output: String) = {
    if (output.length > 20)
      None
    else {
      val end = output.lastIndexOf("ms")
      val start = output.lastIndexOf("\n", end) + 1
      if (start < 0 || end < 0)
        None
      else {
        val s = output.substring(start, end)
        val d = s.toDouble / 1000
        Some(d)
      }
    }
  }

  override def newResultProcessor(outfile: File, defaultTimeout: Int, processLogsOnly: Boolean = false) = PrincessResultProcessor(outfile, defaultTimeout, processLogsOnly)

  case class PrincessResultProcessor(outfile: File, defaultTimeout: Int, processLogsOnly: Boolean = false) extends ResultProcessor(outfile, defaultTimeout, processLogsOnly) {

    var status: ProverStatus = _
    var time: Option[Double] = Some(defaultTimeout)

    var proofBuilder: StringBuilder = _
    private var lemmas: List[String] = null

    override def extractProverResult(s: => String) = {
      try {
        if (s.contains("TIMEOUT"))
          status = Inconclusive("Timeout")
        else if (s.contains("VALID")) {
          status = Proved
        }

        else if (status == Proved) {
          if (s.startsWith("{")) {
            lemmas = s.substring(1, s.indexOf("}")).split(",").toList
          }
        }
      } catch {
        case e: Exception => println(s"Error ${e.getMessage} in $s")
          throw e
      }
    }

    override def buffer[T](f: => T) = f

    // no setup or teardown
    override def err(s: => String) = try {
      super.err(s)
      if (s.contains("ms")) {
        time = tryExtractTimeSeconds(s)
      }
    } catch {
      case e: Exception => println(s"Error ${e.getMessage} in $s")
        throw e
    }

    override def result =
      if (status == null)
        new ProverResult(Inconclusive("Unknown"), Some(defaultTimeout), StringDetails("Inconclusive"))
      else status match {
        case Proved => new ProverResult(Proved, time, StringDetails("", lemmas))
        case Disproved => new ProverResult(Disproved, time, StringDetails("Disproved"))
        case Inconclusive(reason) => new ProverResult(Inconclusive(reason), time, StringDetails("Inconclusive"))
      }
  }

}
