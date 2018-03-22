package system.optimize

import de.tu_darmstadt.veritas.backend.fof
import de.tu_darmstadt.veritas.backend.fof._
import de.tu_darmstadt.veritas.backend.tff.TffAnnotated
import system.Syntax._
import system.{GenerateTFF, Verification}
import system.Verification.ProofObligation
import veritas.benchmarking

import scala.collection.mutable
import scala.util.Try

object GoalUnpacking {

  class Counter {
    private var _counter = 0
    def next(): Int = {
      val c = _counter
      _counter = _counter + 1
      c
    }
  }

  type CostCount = Map[String, Int]
  def using(rule: Rule, cc: CostCount): CostCount = cc.get(rule.name) match {
    case None => cc + (rule.name -> 1)
    case Some(cost) => cc + (rule.name -> (cost + 1))
  }
  def canAfford(rule: Rule, cc: CostCount): Boolean = cc.get(rule.name) match {
    case None => true
    case Some(cost) => cost < 3
  }

  def nextObligation(goal: Judg, obl: ProofObligation)(implicit counter: Counter) =
    obl.copy(name = s"${obl.name}-${counter.next()}", goals = Seq(goal))
  def nextObligation(goals: Seq[Judg], obl: ProofObligation)(implicit counter: Counter) =
    obl.copy(name = s"${obl.name}-${counter.next()}", goals = goals)

  def unpackObligation(obl: ProofObligation): Seq[ProofObligation] = {
    implicit val counter = new Counter

    obl.goals.flatMap(goal => unpackJudg(goal, obl, Seq(), Map()).resultingObligations(obl, goal))
  }

  private sealed trait Unpacked {
    def resultingObligations(obl: ProofObligation, goal: Judg)(implicit counter: Counter): Seq[ProofObligation]
  }
  private case object Proved extends Unpacked {
    override def resultingObligations(obl: ProofObligation, goal: Judg)(implicit counter: Counter) = Seq()
  }
  private case object DontKnow extends Unpacked {
    override def resultingObligations(obl: ProofObligation, goal: Judg)(implicit counter: Counter) = Seq(nextObligation(Seq(goal), obl))
  }
  private case class Disproved(steps: Seq[(String, Judg)]) extends Unpacked{
    override def resultingObligations(obl: ProofObligation, goal: Judg)(implicit counter: Counter) = throw new MatchError(s"Obligation was disproved:\n${steps.mkString("\n")}")
  }
  private case class ProveThis(obls: Seq[ProofObligation]) extends Unpacked {
    override def resultingObligations(obl: ProofObligation, goal: Judg)(implicit counter: Counter) =
      if (obls.isEmpty)
        Proved.resultingObligations(obl, goal)
      else
        obls
  }

  private def unpackJudg(judg: Judg, obl: ProofObligation, ass: Seq[Judg], cc: CostCount)(implicit counter: Counter): Unpacked = {
    if (obl.assumptions.contains(judg))
      return Proved

    val rules = obl.lang.rules ++ obl.lang.transs.flatMap(_.rules.keys) ++ obl.axioms
    val filteredRules = rules.filter(rule => rule.conclusion.sym == judg.sym && !rule.isLemma)
    val candidates = filteredRules.flatMap{ rule =>
      val freshRule = rule.fresh(obl.gensym)
      Try(freshRule -> freshRule.conclusion.matchTerm(judg)).toOption
    }

    unpackJudgUsingCandidates(judg, obl, ass, cc, candidates)
  }

  private def unpackJudgUsingCandidates(judg: Judg, obl: ProofObligation, ass: Seq[Judg], cc: CostCount, candidates: Seq[(Rule, (Subst, Diff, Int))])(implicit counter: Counter) = {
    if (candidates.size == 0) {
      DontKnow
    }
    else if (candidates.size == 1) {
      // exactly one candidate rule found, applying it
      val (rule, (s, eqs, _)) = candidates.head
      unpackJudgUsingRule(judg, obl, ass, cc, rule, s, eqs)
    }
    else {
      val possibleCandidates = candidates.toStream.filter { case (rule, (s, matchEqs, num)) =>
        possibleCandidate(judg, obl, ass, rule, s, matchEqs, num)
      }
      if (!possibleCandidates.isEmpty && possibleCandidates.tail.isEmpty) {
        // size == 1
        val (rule, (s, eqs, _)) = possibleCandidates.head
        val res = unpackJudgUsingRule(judg, obl, ass, cc, rule, s, eqs)
        res
      }
      else
        ProveThis(Seq(nextObligation(judg, obl)))
    }
  }

  private def unpackJudgUsingRule(judg: Judg, obl: ProofObligation, ass: Seq[Judg], cc: CostCount, rule: Rule, s: Subst, eqs: Diff)(implicit counter: Counter): Unpacked = {
    val asss = ass.map(_.subst(s, true))
    val eqGoals = eqs.map { case (t1, t2) => Judg(equ(t1.sort), t1, t2) }
    val premiseGoals = rule.premises.map(_.subst(s))
    val goals = eqGoals ++ premiseGoals
    val obls = goals.flatMap { prem =>
      unpackJudg(prem, obl, asss ++ (goals diff Seq(prem)), cc) match {
        case Disproved(steps) =>
          return Disproved((rule.name -> judg) +: steps)
        case Proved => Seq()
        case DontKnow => Seq(nextObligation(prem, obl))
        case ProveThis(obls) => obls
      }
    }
    if (obls.isEmpty)
      return Proved
    val mergedObls = mergeExistentialObligations(judg, obls)
    ProveThis(mergedObls)
  }

  def possibleCandidate(judg: Judg, obl: ProofObligation, ass: Seq[Judg], rule: Rule, s: Subst, matchEqs: Diff, num: Int)(implicit counter: Counter): Boolean = {
    // this candidate is possible if we fail to prove the match is impossible
    // that is, this candidate is possible if negatedEqs is not Proved
    val eqPremises = rule.premises.flatMap { j => if (j.sym.isEq || j.sym.isNeq) Some(j.subst(s)) else None }
    val applicabilityConditions = matchEqs.map { case (l, r) => Judg(equ(l.sort), l, r) } ++ eqPremises

    if (applicabilityConditions.isEmpty)
      return true

    val disproved = disproveApplicability(applicabilityConditions, judg, rule, obl, ass)
    !disproved
  }

  /*
   * Merges those obligations that have overlapping sets of existentially quantified variables resulting from unpacking.
   */
  def mergeExistentialObligations(judg: Judg, goals: Seq[ProofObligation]): Seq[ProofObligation] = {
    var buckets = mutable.Seq[(mutable.Set[Var], mutable.ListBuffer[ProofObligation])]()
    val judgVars = judg.freevars

    goals.foreach { g =>
      val vars = mutable.Set[Var]()
      g.goals.foreach(vars ++= _.freevars)
      vars --= judgVars
      buckets.find(_._1.exists(v => vars.contains(v))) match {
        case Some((bucketVars, bucket)) =>
          bucketVars ++= vars
          bucket += g
        case None =>
          buckets :+= (vars, mutable.ListBuffer(g))
      }
    }

    val merged = buckets.flatMap { case (vars, obls) =>
      if (vars.isEmpty)
        obls
      else
        Seq(obls.head.copy(existentials = vars.toSet, goals = obls.flatMap(_.goals)))
    }

    merged
  }

  def disproveApplicability(applicabilityConditions: Seq[Judg], judg: Judg, rule: Rule, obl: ProofObligation, ass: Seq[Judg]): Boolean = {
    val tffNoGoal = obl.asTFF.dropRight(1)

    val eqGoalVars = applicabilityConditions.flatMap(_.freevars).toSet
    val eqExVars = eqGoalVars -- judg.freevars
    val eqExVarsCompiled = eqExVars.map(GenerateTFF.compileVar(_, typed = true))
    val altVarsMap = eqExVars.map(v => v -> obl.gensym.freshVar(v.name, v.sort)).toMap
    val altVars = altVarsMap.values.toSeq
    val eqGoalsAlt = applicabilityConditions.map(_.subst(altVarsMap))

    val assumptionVars = (obl.assumptions ++ ass).flatMap(_.freevars).toSet.diff(eqExVars)
    val goalVars = eqGoalVars.diff(eqExVars)
    val universalVars = (assumptionVars.map(GenerateTFF.compileVar(_, typed = true)) ++ goalVars.map(GenerateTFF.compileVar(_, typed = true)))
    val existentialVars = obl.existentials.map(GenerateTFF.compileVar(_, typed = true))

    val assumptionFormula = Parenthesized(And((obl.assumptions ++ ass).distinct.map(GenerateTFF.compileJudg(_))))
    val applicabilityFormula = Exists(eqExVarsCompiled.toSeq, Parenthesized(And(applicabilityConditions.map(GenerateTFF.compileJudg(_)))))
    val goalBody = Parenthesized(Impl(assumptionFormula, Not(applicabilityFormula)))

    val name = s"UNPACK-[${rule.name}]-${obl.name}"
    val tffGoal = TffAnnotated(name, Conjecture,
      ForAll(universalVars.toSeq,
        Exists(existentialVars.toSeq,
          goalBody)))
    val tff = tffNoGoal :+ tffGoal

    lazy val eqResult = Verification.verify(name, tff, mode = "casc", timeout = 2, Verification.vampireSilentArgs)
//    lazy val eqResultSat = Verification.verify("SAT-" + name, tff, mode = "casc_sat", timeout = 0.5, Verification.vampireSilentArgs)
    val proven = eqResult.status == benchmarking.Proved  // || eqResultSat.status == benchmarking.Proved
    proven
  }
}