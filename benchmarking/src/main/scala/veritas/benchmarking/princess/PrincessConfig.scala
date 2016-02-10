package veritas.benchmarking.princess

import java.io.File
import java.util.regex.Pattern

import veritas.benchmarking._

case class PrincessConfig()
    extends ProverConfig {

  def isValid = proverCommand != null

  override val name = s"princess-casc"
  override val proverCommand = findBinaryInPath(s"java")

  override val acceptedFileFormats = Set(".fof", ".tff")

  def makeCall(file: File, timeout: Int, fullLogs: Boolean) = {
    var call = Seq(proverCommand.getAbsolutePath)
    call = call ++ Seq("-Xss20000k", "-Xmx1500m", "-noverify", "-cp", "princess-all-casc.jar", "ap.CmdlMain", "-inputFormat=tptp")
    if (timeout > 0)
      call = call :+ ("-timeout=" + timeout.toString)

    call = call :+ file.getAbsolutePath
    call
  }

  def tryExtractTimeSeconds(output: String) = {
    if(output.length > 20)
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

  override def newResultProcessor(file: File, timeout: Int) = PrincessResultProcessor(file, timeout)

  case class PrincessResultProcessor(file: File, timeout: Int) extends ResultProcessor {

    var status: ProverStatus = _
    var time: Option[Double] = _

    var proofBuilder: StringBuilder = _

    override def out(s: => String) = try {
      if (s.contains("% SZS status Timeout"))
        status = Inconclusive("Timeout")
      else if (s.contains("% SZS status Theorem")) {
        status = Proved
      }
    } catch {
      case e: Exception => println(s"Error ${e.getMessage} in $s")
        throw e
    }

    override def buffer[T](f: => T) = f // no setup or teardown
    override def err(s: => String) = try {
      if (s.contains("ms")) {
        time = tryExtractTimeSeconds(s)
      }
    } catch {
        case e: Exception => println(s"Error ${e.getMessage} in $s")
          throw e
    }

    override def result =
      if (status == null)
        new ProverResult(Inconclusive("Unknown"), time, StringDetails("Inconclusive"))
      else status match {
        case Proved => new ProverResult(Proved, time, StringDetails(""))
        case Disproved => new ProverResult(Disproved, time, StringDetails("Disproved"))
        case Inconclusive(reason) => new ProverResult(Inconclusive(reason), time, StringDetails("Inconclusive"))
      }
  }
}

