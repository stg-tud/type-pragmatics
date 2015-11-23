package veritas.benchmarking

import java.io.File
import veritas.benchmarking.Main.Config

import scala.collection.immutable.ListMap

case class FileSummary(proverConfig: ProverConfig, proverResult: ProverResult, timeSeconds: Double)

case class Summary(config: Config) {
  private var fileSummaries: ListMap[ProverConfig, ListMap[File, FileSummary]] = ListMap()

  def +=(fileResult: (File, FileSummary)): Unit = {
    val proverConfig = fileResult._2.proverConfig
    fileSummaries.get(proverConfig) match {
      case None => fileSummaries += proverConfig -> ListMap(fileResult)
      case Some(results) => fileSummaries += proverConfig -> (results + fileResult)
    }

    val file = fileResult._1
    val res = fileResult._2
    val status = res.proverResult.status
    val logDetail = config.logProof && status == Proved || config.logDisproof && status == Disproved || config.logInconclusive && status.isInstanceOf[Inconclusive]

    if (config.logPerFile || logDetail)
      println(s"Prover ${res.proverConfig.name} finished $file in ${res.timeSeconds.formatted("%.3f")} seconds: ${res.proverResult.status}")
    if (logDetail)
      print(res.proverResult.details.toHumanString)
  }

  def +=(file: File, fileResult: FileSummary): Unit = {
    this += (file -> fileResult)
  }

  def makeSummary: String = {
    val b = StringBuilder.newBuilder
    for ((proverConfig, files) <- fileSummaries) {
      val numFiles = files.size
      val proved = files filter (_._2.proverResult.status == Proved)
      val numProved = proved.size
      val sumProvedTimes = if (proved.isEmpty) 0 else proved map (_._2.timeSeconds) reduce(_+_)
      val avgProvedTimeSeconds = if (numProved == 0) -1 else sumProvedTimes / numProved

      b ++= s"Prover ${proverConfig.name} attempted $numFiles, proved $numProved, average proved time ${avgProvedTimeSeconds.formatted("%.3f")} seconds\n"
    }

    b.toString()
  }

  def makeCSV: String = {
    val sep = ","
    val b = StringBuilder.newBuilder
    def cell(s: String, end: Boolean = false) = {
      b ++= escape(s, sep, "\"")
      if (end) b ++= "\n"
      else b ++= sep
    }
    
    b ++= s"Prover$sep Timeout$sep File$sep Time-milliseconds$sep Status$sep Details\n"
    for ((proverConfig, files) <- fileSummaries;
         (file, res) <- files) {
      cell(res.proverConfig.name)
      cell(config.timeout.toString)
      cell(file.getAbsolutePath)
      cell((res.timeSeconds * 1000).toString)
      cell(res.proverResult.status.toString)
      cell(res.proverResult.details.toString, true)
    }

    b.toString
  }

  def escape(s: String, avoid: String, quote: String): String = {
    if (!s.contains(avoid) && !s.contains("\n"))
      s.trim
    else
      quote + s.trim.replace("\"", "\\\"").replace("\n", "\t\t") + quote
  }

  import info.folone.scala.poi.Workbook
  def makeXLS: Workbook = {
    import info.folone.scala.poi._

    println(s"file summaries: ${fileSummaries.size}")

    var sheets = Set[Sheet]()
    for ((proverConfig, files) <- fileSummaries) {

      val header = Row(0) { Set(
        StringCell(0, "Prover Config"),
        StringCell(1, "Prover Timeout"),
        StringCell(2, "Veritas Config"),
        StringCell(3, "File"),
        StringCell(4, "Time-ms"),
        StringCell(5, "Status"),
        StringCell(6, "Details")
      )}

      def extractVeritasConfig(file: File): String = {
        //does this work on Windows? it should
        val pathParts = file.getAbsolutePath.split(File.separator)
        pathParts(pathParts.length - 2) //get second last element
      }

      var rows = Set(header)
      var rowNum = 1
      for ((file, res) <- files) {
        val  detailsString = res.proverResult.details.toHumanString
        val cells = Set[Cell](
          StringCell(0, res.proverConfig.name),
          NumericCell(1, config.timeout),
          StringCell(2, extractVeritasConfig(file)),
          StringCell(3, file.getName),
          NumericCell(4, res.timeSeconds * 1000.0),
          StringCell(5, res.proverResult.status.toString),
          StringCell(6, detailsString.replace("\n","\t").substring(0, Math.min(detailsString.length, 32767)))
        )
        rows += Row(rowNum) { cells }
        rowNum += 1
      }

      val sheet = Sheet(proverConfig.name) { rows }
      println(s"new sheet ${proverConfig.name}")
      sheets += sheet
    }

    Workbook(sheets)
  }
}
