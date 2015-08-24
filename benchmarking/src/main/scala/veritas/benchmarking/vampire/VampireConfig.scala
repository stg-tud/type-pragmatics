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

  def tryFindModel(file: File, timeout: Int): String = {
    val call = makeSatCall(file, timeout)
    val buf = new StringBuffer
    val (satout, _) = Runner.exec(call, false, new ResultProcessor {
      var modelBuilder: StringBuilder = null
      var model: String = null

      override def result = new ProverResult(null, None, model)

      override def out(s: => String) =
        if (s.endsWith("Satisfiable!")) {
          modelBuilder = StringBuilder.newBuilder
          model = null
        }
        else if (modelBuilder != null && s.contains("-------")) {
          model = modelBuilder.toString
          modelBuilder = null
        }
        else if (modelBuilder != null) {
          modelBuilder ++= s
        }
      override def buffer[T](f: => T) = f
      override def err(s: => String) = {} // ignore
    })

    satout.details.asInstanceOf[String]
  }


  override def newResultProcessor(file: File, timeout: Int) = VampireResultProcessor(file, timeout)

  case class VampireResultProcessor(file: File, timeout: Int) extends ResultProcessor {
    private val clauses = collection.mutable.ArrayBuffer[VampireClause]()

    var status: ProverStatus = null
    var time: Option[Double] = None

    var proofBuilder: StringBuilder = null
    var proof: String = null
    var model = "no counter example found"
    var terminationReason = null

    override def out(s: => String) = {
      // parse status
      if (s.endsWith("Termination reason: Refutation"))
        status = Proved
      else if (s.contains("Termination reason: Satisfiable")) {
        status = Disproved
        val foundModel = tryFindModel(file, timeout)
        if (foundModel != null)
          model = foundModel
      }
      else if (s.contains("Termination reason: ")) {
        val start = s.indexOf("Termination reason: ") + "Termination reason: ".length
        status = Inconclusive(s.substring(start))
      }

      // parse proof
      else if (s.contains("start Proof")) {
        proofBuilder = StringBuilder.newBuilder
        proof = null
      }
      else if (s.contains("end Proof")) {
        proof = proofBuilder.toString
        proofBuilder = null
      }
      else if (proofBuilder != null) {
        proofBuilder ++= s
      }

      // parse time
      else if (s.contains(" in time ")) {
        val start = s.indexOf(" in time ")
        val end = s.indexOf(" s", start)
        if (start < 0 || end < 0)
          time = None
        else {
          val timeS = s.substring(start + " in time ".length, end)
          val timeD = timeS.toDouble
          time = Some(timeD)
        }
      }
    }

    override def buffer[T](f: => T) = f // not setup or teardown
    override def err(s: => String) = {} // ignore

    override def result =
      if (status == null)
        new ProverResult(Inconclusive("Unknown"), time, VampireTrace(clauses))
      else status match {
        case Proved => new ProverResult(Proved, time, proof)
        case Disproved => new ProverResult(Disproved, time, model)
        case Inconclusive(reason) => new ProverResult(Inconclusive(reason), time, VampireTrace(clauses))
      }
  }
}

case class VampireClause(term: String, saNew: Int, saActive: Int, saPassive: Int)
case class VampireTrace(clauses: collection.mutable.ArrayBuffer[VampireClause])
