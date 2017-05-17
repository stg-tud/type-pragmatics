package de.tu_darmstadt.veritas.sudoku

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{Verifier, VerifierHints}

/**
  * Trying to apply Z3 to verify a Sudoku directly
  */
class SudokuLeafVerifier extends Verifier[EmptySpec, SudokuField] {
  override type V = Nothing //??? no particular verification format needed

  /** Textual description that should be unique (used for ordering verifiers) */
  override val desc: String = "Sudoku leaf verifier"

  override def verify[Result <: GenStepResult[EmptySpec, SudokuField]](goal: SudokuField,
                                                                       spec: EmptySpec,
                                                                       parentedges: Iterable[EdgeLabel],
                                                                       assumptions: Iterable[SudokuField],
                                                                       hints: Option[VerifierHints],
                                                                       produce: StepResultProducer[EmptySpec, SudokuField, Result]): Result = ???
}
