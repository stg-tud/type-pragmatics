package veritas.benchmarking

import java.io.File

import veritas.benchmarking.Main.Config

case class Runner(config: Config) {

  val summary = new Summary(config)

  def listAllFiles(f: File): Array[File] =
    if (f.isFile && f.getName.endsWith(".fof"))
      Array(f)
    else if (f.isDirectory)
      f.listFiles().flatMap(listAllFiles(_))
    else
      Array()

  lazy val allFiles = config.files.flatMap(listAllFiles(_))

  def run(): Unit = {
    for (proverConfig <- config.proverConfigs) {
      for (file <- allFiles) {
        val call = proverConfig.makeCall(file, config.timeout)
        val (result, proctime) = Runner.exec(call, config.timeout, config.logExec, () => proverConfig.newResultProcessor(file, config.timeout))
        val tooltime = result.timeSeconds
        val time = tooltime match {
          case None => proctime
          case Some(t) =>
            val diff = Math.abs(proctime - t)
            if (diff > 0.1)
              println(s"WARNING: measured time ${proctime.formatted("%.3f")} differs from tool time $t by ${diff.formatted("%.3f")}.")
            t
        }

        summary += file -> FileSummary(proverConfig, result, time)
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
