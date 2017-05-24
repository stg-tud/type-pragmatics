package de.tu_darmstadt.veritas.sudoku.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.{Tactic, TacticApplicationException}
import de.tu_darmstadt.veritas.sudoku.{EmptySpec, SudokuField}

/**
  * Created by sylvia on 24.05.17.
  */
object NoSingleCandidateFound extends TacticApplicationException[EmptySpec, SudokuField] {
  override val tactic: Tactic[EmptySpec, SudokuField] = SolveSingleCandidate
}
