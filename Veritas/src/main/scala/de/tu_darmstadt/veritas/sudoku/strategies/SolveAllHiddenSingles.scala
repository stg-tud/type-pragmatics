package de.tu_darmstadt.veritas.sudoku.strategies

import de.tu_darmstadt.veritas.sudoku.{EmptySpec, SudokuField}
import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals, Strategy}
import de.tu_darmstadt.veritas.sudoku.tactics._

/**
  * Solve All Hidden Singles within a given Sudoku field
  */
class SolveAllHiddenSingles extends Strategy[EmptySpec, SudokuField] {
  override def applyToPG(pg: ProofGraph[EmptySpec, SudokuField] with ProofGraphTraversals[EmptySpec, SudokuField])
                        (obl: pg.Obligation): ProofGraph[EmptySpec, SudokuField] = {
    try {
      val ps = pg.applyTactic(obl, RuleOutCandidatesSimple)
      val subobls = pg.requiredObls(ps)
      val subobl = subobls.head._1 //assume that there is always only a single subobligation

      try {
        val ps2 = pg.applyTactic(subobl, SolveSingleCandidate)
        val nextsubobl = pg.requiredObls(ps2)
        //try another iteration
        this.applyToPG(pg)(nextsubobl.head._1)
      } catch {
        case NoSingleCandidateFound => {
          pg.unapplyTactic(subobl)
          //add tactic for solving leaves per default
          pg.applyTactic(subobl, SolveSudoku)
          pg
        }
      }
    } catch {
      case NoCandidateCanBeRuledOut => {
        pg.unapplyTactic(obl)
        //try to look for another hidden single
        try {
          val ps = pg.applyTactic(obl, SolveSingleCandidate)
          val subobls = pg.requiredObls(ps)
          val subobl = subobls.head._1 //assume that there is always only a single subobligation
          //try another iteration if successful
          this.applyToPG(pg)(subobl)

        } catch {
          case NoSingleCandidateFound => {
            pg.unapplyTactic(obl)
            //add tactic for solving leaves per default
            pg.applyTactic(obl, SolveSudoku)
            pg
          }
        }
      }
    }
  }
}
