package veritas.benchmarking.princess

import java.io.File
import java.util.regex.Pattern

import veritas.benchmarking._

case class PrincessConfig()
    extends ProverConfig {

  def isValid = proverCommand != null

  override val name = s"princess"
  override val proverCommand = findBinaryInPath(s"java")

  override val acceptedFileFormats = Set(".fof", ".tff")

  def makeCall(file: File, timeout: Int, fullLogs: Boolean) = {
    var call = Seq(proverCommand.getAbsolutePath)
    call = call ++ Seq("-Xss20000k", "-Xmx1500m", "-noverify", "ap.CmdlMain", "-inputFormat=tptp")
    if (timeout > 0)
      //java -Xss20000k -Xmx1500m -noverify ap.CmdlMain -printTree -inputFormat=tptp -timeout=%1 %2
      call = call :+ ("-timeout=" + timeout.toString)


    if (fullLogs) {
      call = call :+ "+printTree" // doesn't seem to work.
    }
    call = call :+ file.getAbsolutePath
    println(call)
    call
  }

  def tryExtractTimeSeconds(output: String) = {
    if(output.length > 20)
      None
    else {
      println("extracting time")
      val end = output.lastIndexOf("ms")
      val start = output.lastIndexOf("\n", end) + 1
      if (start < 0 || end < 0)
        None
      else {
        val s = output.substring(start, end)
        val d = s.toDouble / 1000
        println(d)
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
      //println(s)
      if (s.contains("% SZS status TimeOut"))
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
        println(s)
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

