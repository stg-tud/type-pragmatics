package de.tu_darmstadt.veritas.sudoku.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.strategies.Strategy
import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.sudoku.tactics._
import de.tu_darmstadt.veritas.sudoku.{EmptySpec, SudokuField}

/**
  * Created by sylvia on 26.05.17.
  */
class SolveOneHiddenSingle extends Strategy[EmptySpec, SudokuField] {
  override def applyToPG(pg: ProofGraph[EmptySpec, SudokuField] with ProofGraphTraversals[EmptySpec, SudokuField])
                        (obl: pg.Obligation): ProofGraph[EmptySpec, SudokuField] = {
    try {
      val ps = pg.applyTactic(obl, RuleOutCandidatesSimple)
      val subobls = pg.requiredObls(ps)
      val subobl = subobls.head._1 //assume that there is always only a single subobligation

      try {
        val ps2 = pg.applyTactic(subobl, SolveSingleCandidate)
        val nextsubobl = pg.requiredObls(ps2)
        //this cannot throw an exception if a candidate was solved? not sure
        try {
          val ps3 = pg.applyTactic(nextsubobl.head._1, RuleOutCandidatesSimple)
          //mark resulting subobligation with SolveSudoku tactic
          val finalsubobl = pg.requiredObls(ps3).head._1
          pg.applyTactic(finalsubobl, SolveSudoku)
          pg
        } catch {
          case NoCandidateCanBeRuledOut => {
            pg.unapplyTactic(nextsubobl.head._1)
            pg.applyTactic(nextsubobl.head._1, SolveSudoku)
            //throw NoCandidateCanBeRuledOut
            pg
          }
        }
      } catch {
        case NoSingleCandidateFound => {
          pg.unapplyTactic(subobl)
          pg.applyTactic(subobl, SolveSudoku)
          //throw NoSingleCandidateFound
          pg
        }
      }
    } catch {
      case NoCandidateCanBeRuledOut => {
        pg.unapplyTactic(obl)
        //throw NoCandidateCanBeRuledOut
        pg
      }
    }

  }

}
