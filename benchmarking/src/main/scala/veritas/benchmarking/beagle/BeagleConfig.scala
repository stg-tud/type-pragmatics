package veritas.benchmarking.beagle

import java.io.File

import veritas.benchmarking._

case class BeagleConfig()
  extends ProverConfig {

  def isValid = proverCommand != null

  override val name = s"beagle"
  override val proverCommand = findBinaryInPath(s"java")

  //val jar = findFileInPath("beagle.jar")

  override val acceptedFileFormats = Set(".fof", ".tff")

  def makeCall(file: File, timeout: Int, fullLogs: Boolean) = {
    var call = Seq(proverCommand.getAbsolutePath)
    call = call ++ Seq("-jar", "beagle.jar")
    if (timeout > 0)
      call = call ++ Seq("-t", timeout.toString)

    call = call :+ "-proof"
    call = call :+ file.getAbsolutePath
    println(call)
    call
  }

  override def newResultProcessor(outfile: File, defaultTimeout: Int, processLogsOnly: Boolean = false) = BeagleResultProcessor(outfile, defaultTimeout, processLogsOnly)

  case class BeagleResultProcessor(outfile: File, defaultTimeout: Int, processLogsOnly: Boolean = false) extends ResultProcessor(outfile, defaultTimeout, processLogsOnly) {

    var status: ProverStatus = _
    var time: Option[Double] = Some(defaultTimeout)

    var proofBuilder: StringBuilder = _
    var proof: String = null

    private var proofOutputRunning = false

    override def extractProverResult(s: => String) = {
      try {
        println(s)
        if (s.contains("% SZS status Theorem"))
          status = Proved
        else if (s.contains("% SZS status CounterSatisfiable")) {
          status = Disproved
        } else if (status == Proved && s.contains("Refutation")) {
          proofOutputRunning = true
          proofBuilder = StringBuilder.newBuilder
        } else if (proofOutputRunning && s.contains("Inference")) {
          proof = proofBuilder.toString
          proofBuilder = null
          proofOutputRunning = false
        } else if (proofOutputRunning) {
          proofBuilder ++= s
        }
      } catch {
        case e: Exception => println(s"Error ${e.getMessage} in $s")
          throw e
      }
    }

    override def buffer[T](f: => T) = f // no setup or teardown
    override def err(s: => String) = {println(s)}

    override def result =
      if (status == null)
        new ProverResult(Inconclusive("Unknown"), Some(defaultTimeout), StringDetails("Inconclusive"))
      else status match {
        case Proved => new ProverResult(Proved, time, StringDetails(proof))
        case Disproved => new ProverResult(Disproved, time, StringDetails("Disproved"))
        case Inconclusive(reason) => new ProverResult(Inconclusive(reason), time, StringDetails("Inconclusive"))
      }
  }
}

