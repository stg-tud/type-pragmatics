package veritas.benchmarking

import java.io.File
import java.util.{Date, Calendar}
import java.util.concurrent.Executors

import veritas.benchmarking.Main.Config

import scala.collection.parallel.ExecutionContextTaskSupport
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration


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

  private def calcFinishTime(d: Duration): String = {
    val cal = Calendar.getInstance()
    cal.setTime(new Date())
    cal.add(Calendar.SECOND, d.toSeconds.toInt)
    cal.getTime.toString
  }


  def run(): Unit = {
    val joblist = for {proverConfig <- config.proverConfigs
                       file <- allFiles
                       if (proverConfig.acceptedFileFormats.exists(s => file.getName().endsWith(s)))}
      yield (proverConfig, file)

    val estimatedDuration = Duration(joblist.length * config.timeout, "seconds")


    // set up parallel calls to provers (careful, resources may differ significantly from sequential case!)
    if (config.parallelism > 0) {
      val parnum = if (config.parallelism == 1)
      //get number of available system CPUs, substract 1 just in case
        Runtime.getRuntime().availableProcessors() - 1
      else config.parallelism

      println(s"Will execute ${joblist.length} jobs, executing $parnum at a time.")
      println(s"Estimated worst case duration: ${estimatedDuration / parnum}, " +
        s"i.e. would be finished on ${calcFinishTime(estimatedDuration / parnum)}")

      val parjoblist = joblist.par
      val threadPool = Executors.newFixedThreadPool(parnum)
      parjoblist.tasksupport =
        new ExecutionContextTaskSupport(ExecutionContext.fromExecutor(threadPool))

      val summarylist = parjoblist.map { case (pc, file) => callProverAndLog(pc, file) }
      for (fs <- summarylist.toList) {
        summary += fs
      }
      threadPool.shutdown() //without this line, the worker threads stay alive!
    } else {
      //set up sequential execution!
      println(s"Will execute ${joblist.length} jobs sequentially.")
      println(s"Estimated worst case duration: ${estimatedDuration}, " +
        s"i.e. would be finished on ${calcFinishTime(estimatedDuration)}")
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
