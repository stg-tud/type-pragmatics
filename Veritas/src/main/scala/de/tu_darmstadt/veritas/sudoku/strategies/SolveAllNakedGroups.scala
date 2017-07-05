package de.tu_darmstadt.veritas.sudoku.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals, Strategy}
import de.tu_darmstadt.veritas.sudoku.tactics.{NakedGroup, NoNewNakedGroupOfSizeNFound, SolveSudoku}
import de.tu_darmstadt.veritas.sudoku.{EmptySpec, SudokuField}

/**
  * Solve all naked groups, starting with small groups and counting size up until "until".
  * In between, solve all hidden singles
  */
class SolveAllNakedGroups(until: Int) extends Strategy[EmptySpec, SudokuField] {
  val solveSingles = new SolveAllHiddenSingles
  val nakedGroups = (for (i <- 2 to until) yield i -> new NakedGroup(i)).toMap

  override def applyToPG(pg: ProofGraph[EmptySpec, SudokuField] with ProofGraphTraversals[EmptySpec, SudokuField])
                        (obl: pg.Obligation): ProofGraph[EmptySpec, SudokuField] = {
    val pg_solved_singles = solveSingles.applyToPG(pg)(obl)
    //assume that the next leaf is always the very last obligation returned by DFS traversal
    val nextleaf: pg.Obligation = pg.obligationDFS().last
    var n = 2
    var stop = false
    while (n <= until && !stop) {
      try {
        //will override previous tactic in nextleaf
        val solvegroupn_ps = pg.applyTactic(nextleaf, nakedGroups(n))
        val intermediateleaves = pg.requiredObls(solvegroupn_ps)
        val intermediateleaf = intermediateleaves.head._1
        stop = true
        applyToPG(pg)(intermediateleaf)
      } catch {
        case NoNewNakedGroupOfSizeNFound(n1) => {
          stop = false
          n += 1
        }
      }
    }
    //in the end, make sure that the final leaf contains the default tactic
    val finalleaf: pg.Obligation = pg.obligationDFS().last
    pg.applyTactic(finalleaf, SolveSudoku)
    pg
  }
}
