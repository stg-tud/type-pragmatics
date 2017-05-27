package de.tu_darmstadt.veritas.sudoku.tactics
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.TacticApplicationException
import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenObligation, ObligationProducer}
import de.tu_darmstadt.veritas.sudoku.{EmptySpec, SudokuCell, SudokuField}

/**
  * look for the very first cell that has a single candidate and fills it out
  * throws an exception if no single candidate was found
  */
object SolveSingleCandidate extends SudokuTactic {
  /**
    * applying a tactic to a ProofStep returns the edges generated from this application
    * edges include edge labels and sub-ProofSteps
    * caller has to decide whether the edges will be integrated into a proof graph or not
    *
    * @param obl
    * @param obllabels labels from edges that lead to the given obligation (for propagating proof info if necessary)
    * @throws TacticApplicationException
    * @return
    */
  override def apply[Obligation](obl: GenObligation[EmptySpec, SudokuField],
                                 obllabels: Iterable[EdgeLabel],
                                 produce: ObligationProducer[EmptySpec, SudokuField, Obligation]): Iterable[(Obligation, EdgeLabel)] = {
    val sudokuField = obl.goal
    val singleCandidate = sudokuField.findFirstCellWhere(c => c.candidates.size == 1)
    singleCandidate match {
      case None => throw NoSingleCandidateFound
      case Some((pos, cand)) => {
        val newcell = SudokuCell(cand.candidates.head, Set())
        val newfield = sudokuField.updateSudokuField(Seq((pos, newcell)))
        val edge = FillSingleCandidate(pos, newcell.value)
        Seq((produce.newObligation(obl.spec, newfield), edge))
      }
    }
  }

  override def toString: String = "Solving a hidden single."
}
