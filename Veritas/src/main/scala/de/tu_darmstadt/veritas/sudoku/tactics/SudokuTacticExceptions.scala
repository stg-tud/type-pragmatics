package de.tu_darmstadt.veritas.sudoku.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.{Tactic, TacticApplicationException}
import de.tu_darmstadt.veritas.sudoku.{EmptySpec, SudokuField}

/**
  * tactic application exception for when no single candidate can be found
  */
object NoSingleCandidateFound extends TacticApplicationException[EmptySpec, SudokuField] {
  override val tactic: Tactic[EmptySpec, SudokuField] = SolveSingleCandidate
}

object NoCandidateCanBeRuledOut extends TacticApplicationException[EmptySpec, SudokuField] {
  override val tactic: Tactic[EmptySpec, SudokuField] = RuleOutCandidatesSimple
}

case class NoNewNakedGroupOfSizeNFound(n: Int) extends TacticApplicationException[EmptySpec, SudokuField] {
  override val tactic: Tactic[EmptySpec, SudokuField] = new NakedGroup(n)
}
