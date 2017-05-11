package de.tu_darmstadt.veritas.sudoku.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.Tactic
import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, Strategy}
import de.tu_darmstadt.veritas.sudoku.SudokuField

/**
  * tries to apply a single given tactic to the root of the Sudoku PG
  * returns graph unchanged if tactic application did not succeed
  */
class ApplySingle(t: Tactic[SudokuField, SudokuField]) extends Strategy[SudokuField, SudokuField] {
  override def applyToPG(pg: ProofGraph[SudokuField, SudokuField]): ProofGraph[SudokuField, SudokuField] = {
    val ps = pg.applyTactic(pg.findObligation("initial").get, t)
    pg
  }
}
