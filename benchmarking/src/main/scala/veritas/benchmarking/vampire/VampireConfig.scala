package veritas.benchmarking.vampire

import java.io.File
import java.util.regex.Pattern

import veritas.benchmarking._
import veritas.benchmarking.util.GrowingArray

case class VampireConfig(version: String, mode: String = "casc", traceAnalyses: collection.mutable.Seq[VampireTraceAnalisis] = VampireTraceAnalisisOptions.analysisSeq) extends ProverConfig {
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
    call = call ++ Seq("--show_new", "on")
    call = call ++ Seq("--show_active", "on")
    call = call ++ Seq("--show_passive", "on")

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

  def tryFindModel(file: File, timeout: Int): ResultDetails = {
    val call = makeSatCall(file, timeout)
    val buf = new StringBuffer
    val (satout, _) = Runner.exec(call, timeout, false, new ResultProcessor {
      var modelBuilder: StringBuilder = null
      var model: String = null

      override def result = new ProverResult(null, None, StringDetails(model))

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

    satout.details
  }


  override def newResultProcessor(file: File, timeout: Int) = VampireResultProcessor(file, timeout)

  val parenDigits = Pattern.compile("\\(\\d+")
  val braceDigits = Pattern.compile("\\{\\d+")

  case class VampireResultProcessor(file: File, timeout: Int) extends ResultProcessor {

    private var clauses = new GrowingArray[VampireClause](1000)
    private var maxindex = -1

    var status: ProverStatus = null
    var time: Option[Double] = None

    var proofBuilder: StringBuilder = null
    var proof: String = null
    var model: ResultDetails = StringDetails("no counter example found")
    var terminationReason = null

    override def out(s: => String) = try {
//      println(s)

      if (s.contains("Time limit reached!")) {
        // do nothing
      }

      // parse status
      else if (s.endsWith("Termination reason: Refutation"))
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

      // parse clauses
      else if (s.startsWith("[SA] new: ")) {
        val (id, term, age, weight) = parseClause(s, "[SA] new: ")
        addClause(NEW, id, term, age, weight)
      }
      else if (s.startsWith("[SA] active: ")) {
        val (id, term, age, weight) = parseClause(s, "[SA] active: ")
        addClause(ACTIVE, id, term, age, weight)
      }
      else if (s.startsWith("[SA] passive: ")) {
        val (id, term, age, weight) = parseClause(s, "[SA] passive: ")
        addClause(PASSIVE, id, term, age, weight)
      }
    } catch {
      case e: Exception => println(s"Error ${e.getMessage} in $s")
        throw e
    }

    def parseClause(s: String, prefix: String): (Int, String, Int, Int) = {
      var idend = s.indexOf('.')
      if (idend < 0)
        idend = s.length
      val id = s.substring(prefix.length, idend).toInt

      val termprefix = s.substring(idend + 1)

      val termendParenMatcher = parenDigits.matcher(termprefix)
      val parenStart = if (termendParenMatcher.find()) termendParenMatcher.start() else Int.MaxValue
      val termendBraceMatcher = braceDigits.matcher(termprefix)
      val braceStart = if (termendBraceMatcher.find()) termendBraceMatcher.start() else Int.MaxValue
      var squareStart = s.indexOf('[')
      if (squareStart <= 0)  squareStart = s.length + 1

      val termend = Math.min(parenStart, Math.min(braceStart, squareStart)) - 1
      val term = termprefix.substring(0, termend)

      val (age, weight) =
        if (parenStart == Int.MaxValue)
          (-1, -1)
        else {
          val restTerm = termprefix.substring(parenStart + 1)
          val sep = restTerm.indexOf(':')
          val end1 = restTerm.indexOf(":", sep+1)
          val end2 = restTerm.indexOf(")", sep+1)
          val end =
            if (end1 >= 0)
              end1
            else if (end2 >= 0)
              end2
            else
              restTerm.length

          val age = restTerm.substring(0, sep).toInt
          val weight = restTerm.substring(sep + 1, end).toInt
          (age, weight)
        }

      (id, term, age, weight)
    }

    val NEW = 0
    val ACTIVE = 1
    val PASSIVE = 2
    def addClause(kind: Int, id: Int, term: String, age: Int, weight: Int): Unit = {
      var clause = clauses(id)
      if (clause == null) {
        clause = VampireClause(term, age, weight, 0, 0, 0)
        clauses(id) = clause
      }

      kind match {
        case NEW => clause.saNew += 1
        case ACTIVE => clause.saActive += 1
        case PASSIVE => clause.saPassive += 1
      }
    }

    override def buffer[T](f: => T) = f // not setup or teardown
    override def err(s: => String) = {println(s)} // ignore

    override def result =
      if (status == null)
        new ProverResult(Inconclusive("Unknown"), time, VampireTrace(clauses.finalizedArray, VampireConfig.this))
      else status match {
        case Proved => new ProverResult(Proved, time, StringDetails(proof))
        case Disproved => new ProverResult(Disproved, time, model)
        case Inconclusive(reason) => new ProverResult(Inconclusive(reason), time, VampireTrace(clauses.finalizedArray, VampireConfig.this))
      }
  }
}

