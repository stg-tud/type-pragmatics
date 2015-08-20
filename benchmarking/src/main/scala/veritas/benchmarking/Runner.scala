package veritas.benchmarking

import java.io.File

import veritas.benchmarking.Main.Config

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

  def run(): Unit = {
    for (file <- allFiles) {
      val call = config.proverConfig.makeCall(file, config.timeout)
      val (output, proctime) = exec(call, config.logExec)
      val result = config.proverConfig.analyzeOutput(output)
      val tooltime = config.proverConfig.tryExtractTimeSeconds(output)
      val time = tooltime match {
        case None => proctime
        case Some(t) =>
          val diff = Math.abs(proctime - t)
          if (diff > 0.1)
            println(s"WARNING: measured time ${proctime.formatted("%.3f")} differs from tool time $t by ${diff.formatted("%.3f")}.")
          t
      }

      summary += file -> FileSummary(result, time)
//      println(s"Prover ${config.proverConfig.name} finished $file in ${time.formatted("%.3f")} seconds: ${result.status}")
    }
  }


  def exec(call: Seq[String], logExec: Boolean): (String, Double) = {
    import scala.sys.process._
    val buffer = new StringBuffer

    if (logExec)
      println("Calling " + call.mkString(" "))
    
    val start = System.nanoTime()
    val code   = call.run(BasicIO(false, buffer, None)).exitValue()
    val end = System.nanoTime()

    (buffer.toString, (end - start).toDouble / 1000000000)
  }

}
