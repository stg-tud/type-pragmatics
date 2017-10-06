package de.tu_darmstadt.veritas.sudoku.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure._
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.{Tactic, TacticApplicationException}
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.Verifier
import de.tu_darmstadt.veritas.sudoku.{EmptySpec, SudokuField}

/**
  * Abstract class for Sudoku Tactics
  */
abstract class SudokuTactic extends Tactic[EmptySpec, SudokuField]