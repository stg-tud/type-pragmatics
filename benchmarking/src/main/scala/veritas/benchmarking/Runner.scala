package veritas.benchmarking

import java.io.File

import veritas.benchmarking.Main.Config

import scala.collection.parallel.{ForkJoinTaskSupport, ParSeq}

case class Runner(config: Config) {

  val summary = new Summary(config)

  def listAllFiles(f: File): Array[File] =
    if (f.isFile)
      Array(f)
    else if (f.isDirectory)
      f.listFiles().flatMap(listAllFiles(_))
    else
      Array()

  lazy val allFiles = config.files.flatMap(listAllFiles(_))

  private def callProverAndLog: (ProverConfig, File) => (File, FileSummary) = {
    case (proverConfig, file) => {
      val call = proverConfig.makeCall(file, config.timeout, config.fullLogs)
      val (result, proctime) = Runner.exec(call, config.timeout, config.logExec,
        () => proverConfig.newResultProcessor(file, config.timeout))
      val tooltime = result.timeSeconds
      val time = tooltime match {
        case None => proctime
        case Some(t) =>
          val diff = Math.abs(proctime - t)
          if (diff > 0.1)
            println(s"WARNING for prover configuration ${proverConfig.name} and file $file: " +
              s"measured time ${proctime.formatted("%.3f")} " +
              s"differs from tool time $t by ${diff.formatted("%.3f")}.")
          t
      }

      val res = FileSummary(file.getAbsolutePath, proverConfig, result, time)
      val status = res.proverResult.status
      val logDetail = config.logProof && status == Proved ||
        config.logDisproof && status == Disproved ||
        config.logInconclusive && status.isInstanceOf[Inconclusive]
      if (config.logPerFile || logDetail)
        println(s"Prover ${res.proverConfig.name} finished $file in ${res.timeSeconds.formatted("%.3f")} " +
          s"seconds: ${res.proverResult.status}")
      if (logDetail)
        print(res.proverResult.details.toHumanString)


      (file, res)

    }
  }

  def run(): Unit = {
    val joblist = for {proverConfig <- config.proverConfigs
                       file <- allFiles
                       if (proverConfig.acceptedFileFormats.exists(s => file.getName().endsWith(s)))}
      yield (proverConfig, file)


    // set up parallel calls to provers (careful, resources may differ significantly from sequential case!)
    if (config.parallelism > 0) {
      val jobnumber = if (config.parallelism == 1) {
        try {
          val syspar = sys.props("par")
          syspar.toInt
        } catch {
          case e: SecurityException => {
            println("No permissions to read system's parallelism level - defaulting to level 1!");
            config.parallelism
          }
          case e: NumberFormatException => {
            println("Reading system's parallelism level did not yield a number - " +
              "maybe no permissions to read system's parallelism level? Defaulting to level 1!");
            config.parallelism
          }
        }
      } else config.parallelism

      println(s"Executing $jobnumber proof jobs at a time!")

      val parjoblist = joblist.par
      parjoblist.tasksupport = new ForkJoinTaskSupport(
        new scala.concurrent.forkjoin.ForkJoinPool(jobnumber))

      val summarylist = parjoblist.map { case (pc, file) => callProverAndLog(pc, file) }
      for (fs <- summarylist.toList) {
        summary += fs
      }

      //set up sequential execution!
    } else {
      val summarylist = joblist map { case (pc, file) => callProverAndLog(pc, file) }
      for (fs <- summarylist) {
        summary += fs
      }
    }

  }

}

object Runner {
  def exec(call: Seq[String], timeoutSeconds: Int, logExec: Boolean, resultProcessor: () => ResultProcessor): (ProverResult, Double) = {
    import scala.sys.process._

    if (logExec)
      print("Calling " + call.mkString(" ") + " ...")

    val start = System.nanoTime()
    val resultProc = resultProcessor()
    val p = call.run(resultProc)

    import concurrent._
    import ExecutionContext.Implicits.global
    val f = Future(blocking(p.exitValue()))
    try {
      val code = Await.result(f, duration.Duration(timeoutSeconds + 5, "sec"))
      val end = System.nanoTime()

      if (logExec)
        println(" done")

      (resultProc.result, (end - start).toDouble / 1000000000)

    } catch {
      case _: TimeoutException =>
        if (logExec)
          println()
        p.destroy()
        println(s" *** Broked pipe while calling ${call.head}, restarting prover...")
        exec(call, timeoutSeconds, logExec, resultProcessor)
    }

  }
}
