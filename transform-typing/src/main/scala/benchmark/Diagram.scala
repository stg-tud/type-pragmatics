package benchmark

import java.io.{BufferedReader, File, FileReader, PrintWriter}

import benchmark.Benchmark.OptimizationConfig
import org.sameersingh.scalaplot.Style.{Color, PointType}
import org.sameersingh.scalaplot._
import org.sameersingh.scalaplot.gnuplot.GnuplotPlotter
import org.sameersingh.scalaplot.jfreegraph.JFGraphPlotter
import veritas.benchmarking.Main.Config

import scala.collection.mutable

object Diagram extends App {
  case class Row(trans: String, config: OptimizationConfig.Value, obl: String, status: String, time: Double)

  val realColors: Map[OptimizationConfig.Value, String] = Map(
    OptimizationConfig.All -> "#fb8072",
    OptimizationConfig.NoUnpack -> "#8dd3c7",
    OptimizationConfig.NoNormalization -> "#dddd93",
    OptimizationConfig.NoExistentialHints -> "#bebada",
    OptimizationConfig.NoDropUnreachable -> "#80b1d3",
    OptimizationConfig.None -> "#fdb462"
  )
  val encodedColors: Map[OptimizationConfig.Value, Color.Value] = Map(
    OptimizationConfig.All -> Color.Black,
    OptimizationConfig.NoUnpack -> Color.Grey,
    OptimizationConfig.NoNormalization -> Color.Purple,
    OptimizationConfig.NoExistentialHints -> Color.Magenta,
    OptimizationConfig.NoDropUnreachable -> Color.Yellow,
    OptimizationConfig.None -> Color.Cyan
  )

  def trimmed(s: String) = {
    val s2 = s.trim
    if (s2.startsWith("\"") && s2.endsWith("\""))
      s2.substring(1, s2.length-1)
    else
      s2
  }

  def readRows(file: File): List[Row] = {
    val reader = new BufferedReader(new FileReader(file))
    var rows = mutable.Buffer[Row]()
    while (true) {
      val s = reader.readLine()
      if (s == null) {
        reader.close()
        return rows.toList
      }
      val cells = s.split("\t")

      val trans = trimmed(cells(0))
      val config = OptimizationConfig.fromName(trimmed(cells(1)))
      val obl = trimmed(cells(2))
      val status = trimmed(cells(3))
      val time = trimmed(cells(4)).toDouble
      rows += Row(trans, config, obl, status, time)
    }
    ???
  }

  def run(file: File) {
    val rows = readRows(file)
    val trans = rows.head.trans

    val rowsPerConfig = rows.groupBy(_.config)

    val seriesPerConfig = rowsPerConfig.map { case (config, rawrows) =>
      val rows = rawrows.filter(_.status == "Proved")
      val percentile = 1.0 / rawrows.size
      val percentiles = for (i <- 1 to rows.size) yield i * percentile
      val sorted = rows.map(_.time).sorted

      val (restime, restprogres) =
        if (rows.size == rawrows.size) {
          (None, None)
        }
        else {
          (Some(sorted.last * 10), Some(percentiles.last))
        }

      val series = new MemXYSeries(sorted ++ restime, percentiles ++ restprogres, config.toString)
      series.pointSize = Some(0.1)
      series.lineWidth = Some(1)
      series.pointType = Some(PointType.fullBox)
      series.color = Some(encodedColors(config))
      config -> series
    }.toSeq
    val sortedSeries = seriesPerConfig.sortBy(_._1).map(_._2)
    val resortedSeries = sortedSeries.tail :+ sortedSeries.head

    val data = new XYData(resortedSeries: _*)

    val xaxis = new NumericAxis
    xaxis.log
    xaxis.label_=("Time (s)")
    val yaxis = new NumericAxis
    yaxis.range_=((0.0, 1.0))
    yaxis.label_=("Completion (%)")

    val chart = new XYChart(None, data, x = xaxis, y = yaxis)
    chart.showLegend = true
    chart.legendPosX = LegendPosX.Center

    val plotter = new MyGnuplotPlotter(chart)
    plotter.pdf("benchmark/", s"$trans-chart")
  }

  for (file <- args)
    run(new File(file))
}

class MyGnuplotPlotter(chart: Chart) extends GnuplotPlotter(chart) {
  override def writeScriptFile(directory: String, filenamePrefix: String, dummyTerminal: String,  filenameSuffix: String, stdout: Boolean = false, defaultTerminal: String = "dumb") {
    val monochromeString = if (chart.monochrome) "monochrome" else ""
    val sizeString = if (chart.size.isDefined) "size %f,%f" format(chart.size.get._1, chart.size.get._2) else ""
    val terminal = "pdf enhanced linewidth 1.0 %s %s" format(monochromeString, sizeString)

    // write the description
    assert(new File(directory).isDirectory, directory + " should be a directory")
    assert(directory.endsWith("/"), directory + " should end with a /")
    reset
    this.directory = directory
    filename = filenamePrefix
    plotChart(chart, terminal)
    lines += "set terminal %s" format (terminal)
    if (stdout) lines += "set output"
    else lines += "set output \"%s\"" format (filename + "." + filenameSuffix)
    chart match {
      case xyc: XYChart => plotXYChart(xyc)
      case bc: BarChart => plotBarChart(bc)
    }
    lines += "unset output"
    lines += "# Wrapup"
    lines += "set terminal %s" format (defaultTerminal)
    lines += "refresh"

    val scriptFile = directory + filenamePrefix + ".gpl"
    val writer = new PrintWriter(scriptFile)
    for (line <- lines) {
      writer.println(line)
    }
    writer.close()
  }

  override protected def getColorname(color: Color.Type): String = {
    import Diagram._
    realColors(encodedColors.find(_._2 == color).get._1)
  }
}