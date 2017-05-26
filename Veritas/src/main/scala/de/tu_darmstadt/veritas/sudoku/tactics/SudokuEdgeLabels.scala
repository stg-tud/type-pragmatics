package de.tu_darmstadt.veritas.sudoku.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, PropagatableInfo}
import de.tu_darmstadt.veritas.sudoku.{IndexedSudokuUnit, Position}

/**
  * Edge label which explains a step that fills out a single candidate
  */
case class FillSingleCandidate(position: Position, candidate: Int) extends EdgeLabel {
  override def desc: String = s"Fill field $position with single candidate $candidate"

  override def propagateInfoList: Seq[PropagatableInfo] = Seq()

  override def compare(that: EdgeLabel): Int = this.hashCode() compare that.hashCode()
}

case class SimpleRuleOut(updatedcells: IndexedSudokuUnit) extends EdgeLabel {
  override def desc: String = s"Ruled out candidates from ${updatedcells.size} cells (simple)"

  override def propagateInfoList: Seq[PropagatableInfo] = Seq()

  override def compare(that: EdgeLabel): Int = this.hashCode() compare that.hashCode()
}
