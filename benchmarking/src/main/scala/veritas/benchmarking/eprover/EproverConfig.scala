package veritas.benchmarking.eprover

import java.io.File

import veritas.benchmarking._

case class EproverConfig()
  extends ProverConfig {

  def isValid = proverCommand != null

  override val name = s"eprover"
  override val proverCommand = findBinaryInPath(s"eprover")

  override val acceptedFileFormats = Set(".fof")

  def makeCall(file: File, timeout: Int, fullLogs: Boolean) = {
    var call = Seq(proverCommand.getAbsolutePath)
    call = call ++ Seq("--auto", "--tptp3-format", "--resources-info", "--proof-object")
    if (timeout > 0)
      call = call :+ ("--cpu-limit=" + timeout.toString)


    if (fullLogs) {

    }
    call = call :+ file.getAbsolutePath
    call
  }

  def tryExtractTimeSeconds(output: String) = {
    val begin = output.indexOf(":") + 1
    val end = output.indexOf("s", begin) - 1
    if(begin >= 0 && end > 0)
      Some(output.substring(begin, end).toDouble)
    else
      None
  }

  override def newResultProcessor(file: File, timeout: Int) = EproverResultProcessor(file, timeout)

  case class EproverResultProcessor(file: File, timeout: Int) extends ResultProcessor {

    var status: ProverStatus = _
    var time: Option[Double] = Some(0)

    var proofBuilder: StringBuilder = _
    var proof: String = null

    private var proofOutputRunning = false

    private var lemmas = List[String]()

    override def out(s: => String) = try {
      //print(s)
      if (s.contains("# SZS status Theorem"))
        status = Proved
      else if (s.contains("# SZS status CounterSatisfiable")) {
        status = Disproved
      } else if (s.contains("# Failure:")) {
        status = Inconclusive(s.substring(s.indexOf("# Failure:") + "# Failure".length))
      } else if (s.contains("# SZS output start CNFRefutation.")) {
        proofOutputRunning = true
        proofBuilder = StringBuilder.newBuilder
      } else if (s.contains("# SZS output end CNFRefutation.")) {
        proof = proofBuilder.toString
        proofBuilder = null
        proofOutputRunning = false

      } else if (s.contains("Total time")) {
        time = tryExtractTimeSeconds(s)
      }

      if(proofOutputRunning && !s.startsWith("#")) {
        proofBuilder ++= s

        val lemmaregex = """file\('.+',(.+)\)\)""".r.unanchored
        s match {
          case lemmaregex(lemmaname) => lemmas = List(lemmaname) ++ lemmas
          case _ =>
        }
      }
    } catch {
      case e: Exception => println(s"Error ${e.getMessage} in $s")
        throw e
    }

    val NEW = 0
    val ACTIVE = 1
    val PASSIVE = 2


    override def buffer[T](f: => T) = f // no setup or teardown
    override def err(s: => String) = {}

    override def result =
      if (status == null)
        new ProverResult(Inconclusive("Unknown"), time, StringDetails("Inconclusive"))
      else status match {
        case Proved => new ProverResult(Proved, time, StringDetails(proof, lemmas))
        case Disproved => new ProverResult(Disproved, time, StringDetails("Disproved"))
        case Inconclusive(reason) => new ProverResult(Inconclusive(reason), time, StringDetails("Inconclusive"))
      }
  }
}

