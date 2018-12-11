package de.tu_darmstadt.veritas.sudoku.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.strategies.Strategy
import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.sudoku.{EmptySpec, SudokuField}
import de.tu_darmstadt.veritas.sudoku.tactics.{NakedGroup, NoNewNakedGroupOfSizeNFound, SolveSudoku}

/**
  * Created by sylvia on 31.05.17.
  */
class SolveGroupsUntilThreshold(emptycellsthreshold: Int, groupuntil: Int) extends Strategy[EmptySpec, SudokuField] {
  val solveSingles = new SolveSinglesUntilThreshold(emptycellsthreshold)
  val nakedGroups = (for (i <- 2 to groupuntil) yield i -> new NakedGroup(i)).toMap

  override def applyToPG(pg: ProofGraph[EmptySpec, SudokuField] with ProofGraphTraversals[EmptySpec, SudokuField])
                        (obl: pg.Obligation): ProofGraph[EmptySpec, SudokuField] with ProofGraphTraversals[EmptySpec, SudokuField] = {
    val emptycells = (obl.goal.filterCells(obl.goal.indexedcells, c => c.value == 0)).size
    println(emptycells)
    if (emptycells < emptycellsthreshold) {
      pg.applyTactic(obl, SolveSudoku)
      pg
    } else {
      val pg_solved_singles = solveSingles.applyToPG(pg)(obl)
      //assume that the next leaf is always the very last obligation returned by DFS traversal
      val nextleaf: pg.Obligation = pg.obligationDFS().last
      var n = 2
      var stop = false
      while (n <= groupuntil && !stop) {
        try {
          //will override previous tactic in nextleaf
          val solvegroupn_ps = pg.applyTactic(nextleaf, nakedGroups(n))
          val intermediateleaves = pg.requiredObls(solvegroupn_ps)
          val intermediateleaf = intermediateleaves.head._1
          stop = true
          applyToPG(pg)(intermediateleaf)
        } catch {
          case NoNewNakedGroupOfSizeNFound(_) => {
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
}