package de.tu_darmstadt.veritas.sudoku.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, Strategy}
import de.tu_darmstadt.veritas.sudoku.{EmptySpec, SudokuField}

/**
  * Solve all naked groups, starting with small groups and counting size up until "until".
  * In between, solve all hidden singles
  */
class SolveAllNakedGroups(until: Int) extends Strategy[EmptySpec, SudokuField] {
  override def applyToPG(pg: ProofGraph[EmptySpec, SudokuField])(obl: pg.Obligation): ProofGraph[EmptySpec, SudokuField] = {
    ???
  }
}
