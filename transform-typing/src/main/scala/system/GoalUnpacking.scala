package system

import system.Syntax._
import system.Verification.ProofObligation

object GoalUnpacking {

  class Counter {
    private var _counter = 0
    def next(): Int = {
      val c = _counter
      _counter = _counter + 1
      c
    }
  }

  def nextObligation(goal: Judg, obl: ProofObligation)(implicit counter: Counter) =
    obl.copy(name = s"${obl.name}-${counter.next()}", goals = Seq(goal))
  def nextObligation(goals: Seq[Judg], obl: ProofObligation)(implicit counter: Counter) =
    obl.copy(name = s"${obl.name}-${counter.next()}", goals = goals)

  def unpackObligation(obl: ProofObligation): Seq[ProofObligation] = {
    implicit val counter = new Counter
    obl.goals.flatMap(unpackJudg(_, obl))
  }

  def unpackJudg(judg: Judg, obl: ProofObligation)(implicit counter: Counter): Seq[ProofObligation] = {
    val rules = obl.lang.rules ++ obl.lang.transs.flatMap(_.contracts.keys) ++ obl.axioms
    val filteredRules = rules.filter(rule => rule.conclusion.sym == judg.sym && !rule.lemma)
    val candidates = filteredRules.flatMap{ rule =>
      val freshRule = rule.fresh(obl.gensym)
      try { Some(freshRule -> freshRule.conclusion.matchTerm(judg)) }
      catch { case _: MatchError => None }
    }
    if (candidates.size == 0 || candidates.size > 1)
      Seq(nextObligation(judg, obl))
    else {
      val (rule, (s, eqs)) = candidates.head
      val eqGoals = eqs.map { case(t1, t2) => Judg(equ(t1.sort), t1.subst(s), t2) }
      val eqObls = eqGoals.map(nextObligation(_, obl))
      val premiseGoals = rule.premises.map(_.subst(s))
      val premiseObls = premiseGoals.flatMap(unpackJudg(_, obl))
      val obls = eqObls ++ premiseObls
      val (closedObls, openObls) = obls.partition(_.goals.flatMap(_.freevars).toSet.subsetOf(judg.freevars))
      if (openObls.isEmpty)
        eqObls ++ closedObls
      else {
        val mergedObl: ProofObligation = openObls.reduce((g1, g2) => g1.copy(name = s"${obl.name}-open", goals = g1.goals ++ g2.goals))
        val freevars = (mergedObl.goals.flatMap(_.freevars)).toSet.diff(judg.freevars)
        val existentialMergedObl = mergedObl.copy(existentials = mergedObl.existentials ++ freevars)
        eqObls ++ closedObls :+ existentialMergedObl
      }

    }
  }
}