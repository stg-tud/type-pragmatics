package de.tu_darmstadt.veritas.sudoku.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.{Tactic, TacticApplicationException}
import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenObligation, ObligationProducer}
import de.tu_darmstadt.veritas.sudoku._

/**
  * Tactic for solving situations where
  * there is only one cell within a unit that contains this candidate
  *
  * assumes that candidates have been ruled out correctly
  */
object OnlyCellWithCandidate extends SudokuTactic {
  /**
    * applying a tactic to a ProofStep returns the edges generated from this application
    * edges include edge labels and sub-ProofSteps
    * caller has to decide whether the edges will be integrated into a proof graph or not
    *
    * @param obl
    * @param obllabels labels from edges that lead to the given obligation (for propagating proof info if necessary)
    * @throws TacticApplicationException
    * @return
    */
  override def apply[Obligation](obl: GenObligation[EmptySpec, SudokuField],
                                 obllabels: Iterable[EdgeLabel],
                                 produce: ObligationProducer[EmptySpec, SudokuField, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
    val sudokuField = obl.goal
    val cellrange = sudokuField.config.cellrange
    val units: Seq[IndexedSudokuUnit] = sudokuField.indexedRows ++ sudokuField.indexedColumns ++ sudokuField.indexedBoxes

    // given a unit and a cell value, find all cells whose candidates contain the given number
    def cellsWithCandidate(u: IndexedSudokuUnit, i: Int): IndexedSudokuUnit =
      u.filter { case (pos, c) => c.candidates contains i }

    //lazily compute all cells in all units that are the only ones to possess a certain candidate in their unit
    lazy val onlycands: Seq[(Int, IndexedCell)] =
      for (u <- units; i <- cellrange; cwc = cellsWithCandidate(u, i)
           if cwc.size == 1) yield (i, cwc.head)

    if (onlycands.isEmpty)
      throw NoSingleCandidateFound
    else {
      //force computation of first cell that is the only one to have a certain candidate within a unit
      val (cand, (pos, celltoupdate)) = onlycands.head
      val newcell = SudokuCell(cand, Set())
      val newedge = FillSingleCandidate(pos, cand)
      val newfield = sudokuField.updateSudokuField(Seq((pos, newcell)))
      Seq((produce.newObligation(obl.spec, newfield), newedge))
    }
  }

  override def toString: String = "Solving cell: Was the only cell in a unit to contain a given candidate."
}
