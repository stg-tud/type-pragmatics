package veritas.benchmarking

import java.io.File
import veritas.benchmarking.Main.Config
import scala.collection.immutable.TreeMap

case class FileSummary(proverResult: ProverResult, timeSeconds: Double)

case class Summary(config: Config) {
  private var fileSummaries: TreeMap[File, FileSummary] = TreeMap()

  def +=(fileResult: (File, FileSummary)): Unit = {
    fileSummaries += fileResult
  }
  def +=(file: File, fileResult: FileSummary): Unit = {
    fileSummaries += (file -> fileResult)
  }

  def reportForEachFile: String = {
    val buffer = new StringBuffer
    for ((file, res) <- fileSummaries)
      buffer.append(s"Prover ${config.proverConfig.name} finished $file in ${res.timeSeconds.formatted("%.3f")} seconds: ${res.proverResult.status}\n")
    buffer.toString
  }

  def reportSummary: String = {
    val numFiles = fileSummaries.size
    val proved = fileSummaries filter (_._2.proverResult match {case Proved(_) => true; case _ => false})
    val numProved = proved.size
    val sumProvedTimes = proved map (_._2.timeSeconds) reduce(_+_)
    val avgProvedTimeSeconds = sumProvedTimes / numProved

    s"Attempted $numFiles, proved $numProved, average proved time $avgProvedTimeSeconds seconds\n"
  }
}
