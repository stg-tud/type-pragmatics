package de.tu_darmstadt.veritas.sudoku.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure._
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.{Tactic, TacticApplicationException}
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.Verifier
import de.tu_darmstadt.veritas.sudoku.SudokuField

/**
  * Abstract class for Sudoku Tactics
  */
abstract class SudokuTactic extends Tactic[SudokuField, SudokuField] {

  override def compare(that: Tactic[SudokuField, SudokuField]): Int =
    this.hashCode() compare that.hashCode()
}
