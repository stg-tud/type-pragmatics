package veritas.benchmarking


import veritas.benchmarking.Main.Config

import scala.collection.immutable.ListMap

import info.folone.scala.poi._


case class AnalysisHeader(bconfig: Config, proverConfig: ProverConfig, veritasConfig: VeritasConfig) {
  override def toString() = s"${proverConfig.name}, $veritasConfig"

  def getAnalysisHeaderCells(indices: List[Int]): Set[Cell] = Set[Cell](
    StringCell(indices(0), proverConfig.name),
    NumericCell(indices(1), bconfig.timeout),
    StringCell(indices(2), veritasConfig.typing),
    StringCell(indices(3), veritasConfig.transformations)
  )
}

object AnalysisHeader {
  def getAnalysisHeaderCells(): Set[Cell] = Set[Cell](
    StringCell(0, "Prover Config"),
    StringCell(1, "Timeout"),
    StringCell(2, "Veritas Config, typing"),
    StringCell(3, "Veritas Config, transformations")
  )

  val headerlength = 4
}

abstract class DataAnalysis(bconfig: Config, fileSummaries: ListMap[ProverConfig, List[FileSummary]]) {
  val datatitle: String

  val configCombinations: Set[AnalysisHeader] =
    (for ((pc, lfs) <- fileSummaries; fs <- lfs) yield {
      new AnalysisHeader(bconfig, pc, fs.veritasConfig)
    }).toSet

  val resPerConfig: Map[AnalysisHeader, List[FileSummary]] = (for (ah <- configCombinations)
    yield (ah -> fileSummaries(ah.proverConfig).filter(fs => fs.veritasConfig == ah.veritasConfig))).toMap

  def applyPerConfig[A](f: List[FileSummary] => A): Map[AnalysisHeader, A] =
    resPerConfig map { case (ah, lfs) => (ah, f(lfs)) }

  def applyPerConfig2[A](f: (AnalysisHeader, List[FileSummary]) => A): Map[AnalysisHeader, A] =
    resPerConfig map { case (ah, lfs) => (ah, f(ah, lfs)) }

  def resultToString(ah: AnalysisHeader): String

  def resultToCell(index: Int, ah: AnalysisHeader): Cell

  def getAnalysisResultsStrings(): ListMap[AnalysisHeader, String] = {
    val pairs = applyPerConfig2 { case (ah, lfs) =>
      resultToString(ah)
    }
    ListMap(pairs.toSeq: _*)
  }

  def getAnalysisResultsCells(index: Int): ListMap[AnalysisHeader, Cell] = {
    val pairs = applyPerConfig2 { case (ah, lfs) =>
      resultToCell(index, ah)
    }
    ListMap(pairs.toSeq: _*)
  }
}

//sum total number of files per prover and Veritas config
class TotalPerVC(bconfig: Config, fileSummaries: ListMap[ProverConfig, List[FileSummary]])
  extends DataAnalysis(bconfig, fileSummaries) {

  val datatitle = "No. of files"

  val totalFilesPerConfig: Map[AnalysisHeader, Int] =
    applyPerConfig(_.length)

  override def resultToString(ah: AnalysisHeader): String = s"${totalFilesPerConfig(ah)}"

  override def resultToCell(index: Int, ah: AnalysisHeader): Cell =
    NumericCell(index, totalFilesPerConfig(ah))
}


//calculate number of successfully proven files per prover config and per Veritas config
class SuccessfulPerVC(bconfig: Config, fileSummaries: ListMap[ProverConfig, List[FileSummary]])
  extends TotalPerVC(bconfig, fileSummaries) {

  override val datatitle = "No. of successful proofs"

  val provedtest: FileSummary => Boolean = (fs => fs.proverResult.status == Proved)

  val successfulFilesPerConfig: Map[AnalysisHeader, Int] =
    applyPerConfig(lfs => (lfs filter provedtest).length)

  override def resultToString(ah: AnalysisHeader): String = s"${successfulFilesPerConfig(ah)}"

  override def resultToCell(index: Int, ah: AnalysisHeader): Cell =
    NumericCell(index, successfulFilesPerConfig(ah))
}


//calculate percentage rate of successfully proven files
class SuccessfulRatePerVC(bconfig: Config, fileSummaries: ListMap[ProverConfig, List[FileSummary]])
  extends SuccessfulPerVC(bconfig, fileSummaries) {

  override val datatitle = "Rate of successful proofs"

  //rounds rate to one digit after comma
  def calcSuccessRate(ah: AnalysisHeader): Double =
   successfulFilesPerConfig(ah) / totalFilesPerConfig(ah)

  override def resultToString(ah: AnalysisHeader): String =
    s"${calcSuccessRate(ah)}"

  override def resultToCell(index: Int, ah: AnalysisHeader): Cell =
    NumericCell(index, calcSuccessRate(ah))

}

//calculate average proof time for successful proofs
class AverageTimePerVC(bconfig: Config, fileSummaries: ListMap[ProverConfig, List[FileSummary]])
  extends SuccessfulPerVC(bconfig, fileSummaries) {

  override val datatitle = "Average time per successful proof (sec)"

  val successAvgPerConfig: Map[AnalysisHeader, Double] =
    applyPerConfig2((ah, lfs) => {
      val filterProved = lfs filter provedtest
      val ts = filterProved map (fs => fs.timeSeconds)
      ts.sum / successfulFilesPerConfig(ah)
    })


  override def resultToString(ah: AnalysisHeader): String = s"${successAvgPerConfig(ah)}"

  override def resultToCell(index: Int, ah: AnalysisHeader): Cell =
    NumericCell(index, successAvgPerConfig(ah))

}

//calculate minimal number of used lemmas
