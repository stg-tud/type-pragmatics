package veritas.benchmarking


import scala.collection.immutable.ListMap

import info.folone.scala.poi._


case class AnalysisHeader(proverConfig: ProverConfig, veritasConfig: String) {
  override def toString() = s"${proverConfig.name}, $veritasConfig"

  def getAnalysisHeaderCells(indices: List[Int]): Set[Cell] = Set[Cell](
    StringCell(indices(0), proverConfig.name),
    StringCell(indices(1), veritasConfig)
  )
}

object AnalysisHeader {
  val headerlength = 2
}

abstract class DataAnalysis(fileSummaries: ListMap[ProverConfig, List[FileSummary]]) {
  val datatitle: String

  val configCombinations: Set[AnalysisHeader] =
    (for ((pc, lfs) <- fileSummaries; fs <- lfs) yield {
      new AnalysisHeader(pc, fs.veritasConfig)
    }).toSet

  val resPerConfig: Map[AnalysisHeader, List[FileSummary]] = (for (ah <- configCombinations)
    yield (ah -> fileSummaries(ah.proverConfig).filter(fs => fs.veritasConfig == ah.veritasConfig))).toMap


  def getAnalysisResultsStrings(): ListMap[AnalysisHeader, String]

  def getAnalysisResultsCells(index: Int): ListMap[AnalysisHeader, Cell]
}

//calculate number of successfully proven files per prover config and per Veritas config
class successfulPerVC(fileSummaries: ListMap[ProverConfig, List[FileSummary]]) extends DataAnalysis(fileSummaries) {
  val datatitle = "# of successful proofs"

  val totalFilesPerConfig: Map[AnalysisHeader, Int] =
    resPerConfig map { case (ah, lfs) => (ah, lfs.length) }

  val successfulFilesPerConfig: Map[AnalysisHeader, Int] =
    resPerConfig map { case (ah, lfs) => (ah, (lfs filter (fs => fs.proverResult == Proved)).length) }

  override def getAnalysisResultsStrings(): ListMap[AnalysisHeader, String] = {
    val pairs = (for (ah <- resPerConfig.keys)
      yield (ah -> s"${totalFilesPerConfig(ah)}/${successfulFilesPerConfig(ah)}")).toSeq

    ListMap(pairs: _*)
  }


  override def getAnalysisResultsCells(index: Int): ListMap[AnalysisHeader, Cell] = {
    val pairs = (for ((ah, s) <- getAnalysisResultsStrings())
      yield (ah, StringCell(index, s))).toSeq

    ListMap(pairs: _*)
  }
}
