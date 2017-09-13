package de.tu_darmstadt.veritas.sudoku.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.strategies.Strategy
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.Tactic
import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.sudoku.{EmptySpec, SudokuField}

/**
  * tries to apply a single given tactic to the root of the Sudoku PG
  * returns graph unchanged if tactic application did not succeed
  */
class ApplySingle(t: Tactic[EmptySpec, SudokuField]) extends Strategy[EmptySpec, SudokuField] {
  override def applyToPG(pg: ProofGraph[EmptySpec, SudokuField] with ProofGraphTraversals[EmptySpec, SudokuField])
                        (obl: pg.Obligation): ProofGraph[EmptySpec, SudokuField] = {
    val ps = pg.applyTactic(obl, t)
    pg
  }
}
