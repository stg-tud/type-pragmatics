package system.optimize

import system.Syntax._
import system.Verification.ProofObligation

object AssumptionElimination {

  def eliminateObligation(obl: ProofObligation): ProofObligation =
    obl.copy(assumptions = obl.assumptions.filter(canEliminateJudg(_, obl)))

  def canEliminateJudg(judg: Judg, obl: ProofObligation): Boolean =
    ???

}
