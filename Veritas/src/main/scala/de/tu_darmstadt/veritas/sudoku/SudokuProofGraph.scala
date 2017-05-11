package de.tu_darmstadt.veritas.sudoku

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraphXodus, PropertyTypes, Strategy}
import de.tu_darmstadt.veritas.sudoku.tactics.DoNothing

/**
  * controls construction of a sudoku proof graph
  * by applying domain-specific strategies
  */
class SudokuProofGraph(file: File, initialfield: SudokuField, rootstrategy: Strategy[SudokuField, SudokuField]) {

  val g: ProofGraphXodus[SudokuField, SudokuField] =
    new ProofGraphXodus[SudokuField, SudokuField](file)
  SudokuProofGraph.initializeGraphTypes(g)

  val initialobligation: g.Obligation = g.newObligation(initialfield, initialfield)
  g.storeObligation("initial", initialobligation)

  def constructPG(): Unit = rootstrategy.applyToPG(g)

}


object SudokuProofGraph {
  def initializeGraphTypes(g: ProofGraphXodus[SudokuField, SudokuField]) = {
    PropertyTypes.registerPropertyType[SudokuField](g.store)
    PropertyTypes.registerPropertyType[DoNothing.type](g.store)
  }
}
