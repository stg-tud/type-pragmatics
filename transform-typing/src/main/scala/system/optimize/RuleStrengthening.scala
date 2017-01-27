package system.optimize

import system.Syntax._
import system.Verification
import system.Verification.ProofObligation
import veritas.benchmarking.Proved

/*
 * Drop those axiom premises that follow from the current assumptions anyway.
 */
object RuleStrengthening {

  var strengthenedRules = 0

  def strengthenObligation(obl: ProofObligation) =
    obl.copy(axioms = obl.axioms.map(strengthenRule(_, obl)))

  def strengthenRule(r: Rule, obl: ProofObligation): Rule = {
    val remainingPremises = r.premises.filter { premise =>
      val premiseObl = obl.copy(goals = Seq(premise), name = s"STRENGTHEN-${obl.name}")
      val status = Verification.verify(premiseObl, timeout = 0.3).status
      status != Proved
    }
    if (remainingPremises.size < r.premises.size)
      strengthenedRules += 1
    r.copy(premises = remainingPremises)
  }
}