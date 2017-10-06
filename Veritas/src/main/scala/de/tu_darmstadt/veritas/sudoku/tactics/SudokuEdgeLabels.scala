package de.tu_darmstadt.veritas.sudoku.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, PropagatableInfo}
import de.tu_darmstadt.veritas.sudoku.{IndexedSudokuUnit, Position}

/**
  * Edge label which explains a step that fills out a single candidate
  */
case class FillSingleCandidate(position: Position, candidate: Int) extends EdgeLabel {
  override def desc: String = s"Fill field $position with single candidate $candidate"

  override def propagateInfoList: Seq[PropagatableInfo] = Seq()
}

case class SimpleRuleOut(updatedcells: IndexedSudokuUnit) extends EdgeLabel {
  override def desc: String = s"Ruled out candidates from ${updatedcells.size} cells (simple)"

  override def propagateInfoList: Seq[PropagatableInfo] = Seq()
}

case class FoundNakedGroup(n: Int, groupcells: IndexedSudokuUnit, updatedcells: IndexedSudokuUnit) extends EdgeLabel {

  val grouppos = groupcells.map(_._1).mkString(", ")
  val updatedpos = updatedcells.map(_._1).mkString(", ")

  val ruledoutcandidates: String = groupcells.foldLeft[Set[Int]](Set())((acc, icell) => acc union icell._2.candidates).mkString(", ")

  override def desc: String = s"Found naked group of size $n: $grouppos. Ruling out $ruledoutcandidates from cells $updatedpos."

  override def propagateInfoList: Seq[PropagatableInfo] = Seq()
}
