package veritas.benchmarking

import java.io.File
import veritas.benchmarking.Main.Config

import scala.collection.immutable.{Iterable, IndexedSeq, ListMap}

/**
  *
  * @param filePath absolute path of the file for which the result was produced
  *                 (used for extracting Veritas configuration)
  * @param proverConfig
  * @param proverResult
  * @param timeSeconds
  */
case class FileSummary(filePath: String, proverConfig: ProverConfig, proverResult: ProverResult, timeSeconds: Double) {

  val veritasConfig: String = {
    //does this work on Windows? it should
    val pathParts = filePath.split(File.separator)
    pathParts(pathParts.length - 2) //get second last element
  }

}

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
      val sumProvedTimes = if (proved.isEmpty) 0 else proved map (_._2.timeSeconds) reduce (_ + _)
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

      val header = Row(0) {
        Set(
          StringCell(0, "Prover Config"),
          StringCell(1, "Prover Timeout"),
          StringCell(2, "Veritas Config"),
          StringCell(3, "File"),
          StringCell(4, "Time-ms"),
          StringCell(5, "Status"),
          StringCell(6, "Details")
        )
      }

      var rows = Set(header)
      var rowNum = 1
      for ((file, res) <- files) {
        val detailsString = res.proverResult.details.toHumanString
        val cells = Set[Cell](
          StringCell(0, res.proverConfig.name),
          NumericCell(1, config.timeout),
          StringCell(2, res.veritasConfig),
          StringCell(3, file.getName),
          NumericCell(4, res.timeSeconds * 1000.0),
          StringCell(5, res.proverResult.status.toString),
          StringCell(6, detailsString.replace("\n", "\t").substring(0, Math.min(detailsString.length, 32767)))
        )
        rows += Row(rowNum) {
          cells
        }
        rowNum += 1
      }

      val sheet = Sheet(proverConfig.name) {
        rows
      }
      println(s"new sheet ${proverConfig.name}")
      sheets += sheet
    }

    Workbook(sheets)
  }


  def makeXLSOverview: Workbook = {
    import info.folone.scala.poi._
    val contractedFileSummaries: ListMap[ProverConfig, List[FileSummary]]
      = fileSummaries map { case (pc, fsmap) => (pc, fsmap.values.toList) }

    // val overviewData decides which overview data is computed
    val overviewData: List[DataAnalysis] = List(new successfulPerVC(contractedFileSummaries))
    val overviewColTitles = overviewData map (_.datatitle)

    println(s"creating overview of file summaries: ${fileSummaries.size}")
    println(s"overview contains: ${overviewColTitles.mkString(", ")}")

    val startcolindex = AnalysisHeader.headerlength //index at which data elements start after left header
    val colindices = (startcolindex to overviewColTitles.length - 1 + startcolindex)
    val indicestitles = colindices zip overviewColTitles

    //group different overview values (results) together, passing the necessary cell indices
    val resultsAsSeqs =
      (for {index <- colindices
            datacol <- overviewData
      } yield {
        datacol.getAnalysisResultsCells(index).toSeq
      }).flatten

    val groupedResults = resultsAsSeqs.groupBy(_._1) mapValues (_.map(_._2).toSet)
    val resultTable: Map[Set[Cell], Set[Cell]] = groupedResults map { case (h, s) => (h.getAnalysisHeaderCells((0 to startcolindex - 1).toList), s) }

    //create XLS sheet:
    //header row:
    val header = Row(0) {
      Set[Cell](
        StringCell(0, "Prover Config"),
        StringCell(1, "Veritas Config")
      ) ++
        (colindices map { case (n, s) => StringCell(n, s) }).toSet
    }

    var rows = Set(header)
    var rowNum = 1
    for {(header, rescells) <- resultTable} {
      rows += Row(rowNum) {
        header ++ rescells
      }
      rowNum += 1
    }

    val sheet = Sheet("Overview") {
      rows
    }

    Workbook(Set(sheet))
  }
}
