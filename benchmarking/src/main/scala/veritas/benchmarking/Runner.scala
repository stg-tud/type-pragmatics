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

      val (output, time) = exec(config.proverConfig.makeCall(file))
      val result = config.proverConfig.analyzeOutput(output)
      val status = result match {
        case Disproved(_) => "disproved"
        case Inconclusive(_) => "inconclusive"
        case Proved(_) => "proved"
      }
      val tooltime = config.proverConfig.tryExtractTimeSeconds(output)
      tooltime match {
        case None =>
        case Some(t) =>
          val diff = Math.abs(time - t)
          if (diff > 0.001)
            println(s"WARNING: measured time $time differs from tool time $t by ${diff.formatted("%.3f")}.")
      }

      println(s"Prover ${config.proverConfig.name} finished $file in ${time.formatted("%.3f")} seconds: $status")
    }
  }


  def exec(call: Seq[String]): (String, Double) = {
    import scala.sys.process._
    val start = System.nanoTime()
    val output = call.!!
    val end = System.nanoTime()
    (output, (end - start).toDouble / 1000000000)
  }

}
