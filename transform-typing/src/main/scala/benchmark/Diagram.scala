package benchmark

import java.io.{BufferedReader, File, FileReader}

import benchmark.Benchmark.OptimizationConfig
import org.sameersingh.scalaplot.Style.{Color, PointType}
import org.sameersingh.scalaplot._
import org.sameersingh.scalaplot.gnuplot.GnuplotPlotter
import org.sameersingh.scalaplot.jfreegraph.JFGraphPlotter
import veritas.benchmarking.Main.Config

import scala.collection.mutable

object Diagram extends App {
  case class Row(trans: String, config: OptimizationConfig.Value, obl: String, status: String, time: Double)

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
      series.lineWidth = Some(.3)
      series.pointType = Some(PointType.fullBox)
      if (config == OptimizationConfig.All)
        series.color = Some(Color.Magenta)
      else if (config == OptimizationConfig.NoUnpack)
        series.color = Some(Color.Grey)
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

    val chart = new XYChart(Some(trans), data, x = xaxis, y = yaxis)
    chart.showLegend = true
    chart.legendPosX = LegendPosX.Center

    val plotter = new GnuplotPlotter(chart)
    plotter.pdf("benchmark/", s"$trans-chart")
  }

  for (file <- args)
    run(new File(file))
}
