package de.tu_darmstadt.veritas.sudoku.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.TacticApplicationException
import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenObligation, ObligationProducer}
import de.tu_darmstadt.veritas.sudoku._

/**
  * cross-checks candidates with filled in values against the one-rule and removes
  * all candidates that obviously cannot occur
  */
object RuleOutCandidatesSimple extends SudokuTactic {
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

    def newCandidates(pos: Position, ec: SudokuCell): Set[Int] = {
      val allnonemptyPeers: IndexedSudokuUnit = sudokuField.filterCells(sudokuField.allPeers(Seq(pos)), c => c.value != 0)
      val ruleout: Set[Int] = (allnonemptyPeers map { case (_, c) => c.value }).toSet
      ec.candidates diff ruleout
    }


    val emptycells: IndexedSudokuUnit = sudokuField.filterCells(sudokuField.indexedcells, c => c.value == 0)
    val updatedindexedcells: IndexedSudokuUnit = for ((pos, ec) <- emptycells;
                                                      newcand = newCandidates(pos, ec);
                                                      if (newcand != ec.candidates)) yield {
      val newcell = SudokuCell(ec.value, newcand)
      (pos, newcell)
    }

    if (updatedindexedcells.nonEmpty) {
      val newfield = sudokuField.updateSudokuField(updatedindexedcells)
      Seq((produce.newObligation(obl.spec, newfield), SimpleRuleOut(updatedindexedcells)))
    } else {
      throw NoCandidateCanBeRuledOut
    }

  }

  override def toString: String = "Ruling out simple candidates (given by common unit)."
}
