package veritas.benchmarking

import java.io.File

import veritas.benchmarking.Main.Config

class Runner {

  def listAllFiles(f: File): Array[File] =
    if (f.isFile)
      Array(f)
    else if (f.isDirectory)
      f.listFiles().flatMap(listAllFiles(_))
    else
      Array()

  def run(config: Config): Unit = {
    var files = config.files.flatMap(listAllFiles(_))

    for (file <- files) {

      val (output, proctime) = exec(config.proverConfig.makeCall(file, config.timeout))
      val result = config.proverConfig.analyzeOutput(output)
      val status = result match {
        case Disproved(_) => "disproved"
        case Inconclusive(_) => "inconclusive"
        case Proved(_) => "proved"
      }
      val tooltime = config.proverConfig.tryExtractTimeSeconds(output)
      val time = tooltime match {
        case None => proctime
        case Some(t) =>
          val diff = Math.abs(proctime - t)
          if (diff > 0.1)
            println(s"WARNING: measured time ${proctime.formatted("%.3f")} differs from tool time $t by ${diff.formatted("%.3f")}.")
          t
      }

      println(s"Prover ${config.proverConfig.name} finished $file in ${time.formatted("%.3f")} seconds: $status")
    }
  }


  def exec(call: Seq[String]): (String, Double) = {
    import scala.sys.process._
    val buffer = new StringBuffer
    
    val start = System.nanoTime()
    val code   = call.run(BasicIO(false, buffer, None)).exitValue()
    val end = System.nanoTime()

    (buffer.toString, (end - start).toDouble / 1000000000)
  }

}
