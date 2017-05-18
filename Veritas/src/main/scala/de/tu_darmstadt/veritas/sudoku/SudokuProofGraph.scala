package de.tu_darmstadt.veritas.sudoku

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraphXodus, PropertyTypes, Strategy}
import de.tu_darmstadt.veritas.sudoku.tactics.DoNothing

class EmptySpec extends Comparable[EmptySpec] with Serializable {
  override def compareTo(o: EmptySpec): Int = this.hashCode() compare o.hashCode()
}

/**
  * controls construction of a sudoku proof graph
  * by applying domain-specific strategies
  */
class SudokuProofGraph(file: File, initialfield: SudokuField, rootstrategy: Strategy[EmptySpec, SudokuField]) {

  val g: ProofGraphXodus[EmptySpec, SudokuField] =
    new ProofGraphXodus[EmptySpec, SudokuField](file)
  SudokuProofGraph.initializeGraphTypes(g)

  val initialobligation: g.Obligation = g.newObligation(new EmptySpec, initialfield)
  g.storeObligation("initial", initialobligation)

  def constructPG(): Unit = rootstrategy.applyToPG(g)

}


object SudokuProofGraph {
  def initializeGraphTypes(g: ProofGraphXodus[EmptySpec, SudokuField]) = {
    PropertyTypes.registerPropertyType[SudokuField](g.store)
    PropertyTypes.registerPropertyType[EmptySpec](g.store)
    PropertyTypes.registerPropertyType[DoNothing.type](g.store)
  }
}
