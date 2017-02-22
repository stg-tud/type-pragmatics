package system.optimize

import system.Syntax._
import system.Verification.ProofObligation

import scala.util.Try

object ExistentialHints {

  type Hint = (Var, Term)

  def existentialHints(obl: ProofObligation): ProofObligation = {
    val goals = obl.goals.flatMap { goal =>
      val ihRules = obl.axioms.filter(rule => rule.conclusion.sym == goal.sym && rule.isInductionHypothesis)
      val hints = existentialHintsFromIhs(goal, ihRules, obl.existentials)
      val hintJudgs = hints.map { case (v, t) =>
        val eqJudg = Judg(equ(v.sort), v, t)
        val neqJudg = Judg(neq(v.sort), v, t)
        Judg(OR, eqJudg.toApp, neqJudg.toApp)
      }
      Seq(goal) ++ hintJudgs
    }

    obl.copy(goals = goals)
  }

  private def existentialHintsFromIhs(goal: Judg, ihs: Seq[Rule], existentials: Set[Var]): Seq[Hint] = {
    val candidates = ihs.flatMap { rule =>
      assert(rule.freevars.isEmpty, "Induction hypothesis should not have free variables")
      val ihSymName = rule.inductionSymbolName
      val (ihTransCall, callIx) = rule.conclusion.terms.zipWithIndex.find {
        case (App(sym, _), ix) => sym.name == ihSymName
        case _ => false
      }.get
      val oblTransCall = goal.terms(callIx)
      if (oblTransCall != ihTransCall) // require exact match between goal and IH for trans call
        None
      else
        Try(rule -> rule.conclusion.matchTerm(goal)).toOption
    }

    candidates.flatMap { case (rule, (_, diff, _)) =>
      var hints = Seq[Hint]()
      diff.foreach { case (ihTerm, goalTerm) =>
        if (goalTerm.isInstanceOf[Var] && existentials.contains(goalTerm.asInstanceOf[Var]))
          hints = hints :+ (goalTerm.asInstanceOf[Var] -> ihTerm)
      }
      hints
    }
  }
}
