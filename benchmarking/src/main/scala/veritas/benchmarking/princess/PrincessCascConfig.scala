package veritas.benchmarking.princess

import java.io.File
import java.util.regex.Pattern

import veritas.benchmarking._

case class PrincessCascSlurmConfig() extends PrincessCascConfig {
  override val modulesToLoad = Set("java")

  override def createProverCallHHlr(proverpath: String, provercall: Seq[String]) =
    super.createProverCallHHlr("", provercall)

  override def makeCall(file: File, timeout: Int, fullLogs: Boolean) = {
    // TODO: Need a better way than hardcoding path to the princess-all-casc.jar
    var call = Seq("java")
    var pathToPrincessJar = "/home/groups/projects/proj_184/provers/princess-all-casc.jar"
    call = call ++ Seq("-Xss20000k", "-Xmx1500m", "-noverify", "-cp", pathToPrincessJar, "ap.CmdlMain", "-inputFormat=tptp")
    if (timeout > 0)
      call = call :+ ("-timeout=" + timeout.toString)

    call = call :+ file.getAbsolutePath
    call
  }
}

class PrincessCascConfig()
  extends ProverConfig {

  def isValid = proverCommand != null

  override val name = "princess"
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
    val timeregex = """([0-9]+)ms""".r.unanchored
    val provedregex = s""".+:.+proved.+\\(${timeregex.regex}\\)""".r.unanchored

    //try to match "Prover 0: proved (xxxxms)" first, then just xxxxms (from end of file)
    output match {
      case provedregex(time) => Some(time.toDouble / 1000)
      case timeregex(time) => Some(time.toDouble / 1000)
      case _ => None
    }
  }

  override def newResultProcessor(outfile: File, defaultTimeout: Int, processLogsOnly: Boolean = false) = PrincessResultProcessor(outfile, defaultTimeout, processLogsOnly)

  case class PrincessResultProcessor(outfile: File, defaultTimeout: Int, processLogsOnly: Boolean = false) extends ResultProcessor(outfile, defaultTimeout, processLogsOnly) {

    var status: ProverStatus = _
    var time: Option[Double] = Some(defaultTimeout)

    var proofBuilder: StringBuilder = _

    private var lemmas: List[String] = List[String]()

    override def extractProverResult(s: => String) = {
      try {
        if (s.contains("% SZS status Timeout"))
          status = Inconclusive("Timeout")
        else if (s.contains("% SZS status Theorem")) {
          status = Proved
        }
        else if (status == Proved) {
          if (s.startsWith("{")) {
            lemmas = s.substring(1, s.indexOf("}")).split(",").toList
          }
        }
        else if (s.contains("ms")) {
          time = tryExtractTimeSeconds(s)
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

