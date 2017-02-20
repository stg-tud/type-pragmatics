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
    obl.goals.flatMap(goal => unpackJudg(goal, obl, Seq(), Map()) match {
      case Proved => Seq(nextObligation(Seq(), obl))
      case Disproved(steps) => throw new MatchError(s"Obligation was disproved:\n${steps.mkString("\n")}")
      case DontKnow => Seq(nextObligation(Seq(goal), obl))
      case ProveThis(obls) if obls.isEmpty => Seq(nextObligation(Seq(), obl))
      case ProveThis(obls) => obls
    })
  }

  private sealed trait Unpacked
  private case object Proved extends Unpacked
  private case object DontKnow extends Unpacked
  private case class Disproved(steps: Seq[(String, Judg)]) extends Unpacked
  private case class ProveThis(obls: Seq[ProofObligation]) extends Unpacked

  private def trySolveEquation(judg: Judg, obl: ProofObligation)(implicit counter: Counter): Unpacked =
    if (judg.sym.isEq) {
      Try(judg.terms(0).unify(judg.terms(1))).toOption match {
        case None => Disproved(Seq("#EQ" -> judg))
        case Some((s, eqs, _)) if s.isEmpty && eqs.isEmpty => Proved
        case Some((s, eqs, _)) =>
          val sGoals = s.map(kv => Judg(equ(kv._1.sort), kv._1, kv._2)).toSeq
          val eqGoals = eqs.map(lr => Judg(equ(lr._1.sort), lr._1, lr._2))
          ProveThis(Seq(nextObligation(sGoals ++ eqGoals, obl)))
      }
    }
    else if (judg.sym.isNeq && judg.terms(0) == judg.terms(1))
    // terms are syntactically equal => inequality never holds
      Disproved(Seq("#NEQ" -> judg))
    else if (judg.sym.isNeq && Try(judg.terms(0).unify(judg.terms(1))).isFailure)
    // terms are not unifiable => inequality always holds
      Proved
    else
      DontKnow

  private def unpackJudg(judg: Judg, obl: ProofObligation, ass: Seq[Judg], cc: CostCount)(implicit counter: Counter): Unpacked = {
    if (obl.assumptions.contains(judg))
      return Proved

    trySolveEquation(judg, obl) match {
      case result@(Proved | Disproved(_) | ProveThis(_)) => return result
      case DontKnow => // continue
    }

    val rules = obl.lang.rules ++ obl.lang.transs.flatMap(_.rules.keys) ++ obl.axioms
    val filteredRules = rules.filter(rule => rule.conclusion.sym == judg.sym && !rule.isLemma)
    val candidates = filteredRules.flatMap{ rule =>
      val freshRule = rule.fresh(obl.gensym)
      Try(freshRule -> freshRule.conclusion.matchTerm(judg)).toOption
    }

    if (candidates.size == 0) {
      DontKnow
    }
    else if (candidates.size == 1) {
      // exactly one candidate rule found, applying it
      val (rule, (s, eqs, _)) = candidates.head
      unpackJudgUsingRule(judg, obl, ass, cc, rule, s, eqs)
    }
    else {
      val possibleCandidates = candidates.toStream.filter{ case (rule, (s, matchEqs, num)) =>
        possibleCandidate(judg, obl, ass, rule, s, matchEqs, num)
      }
      if (!possibleCandidates.isEmpty && possibleCandidates.tail.isEmpty) { // size == 1
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
    val eqPremises = rule.premises.filter { j => j.sym.isEq || j.sym.isNeq }
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

//  val (closedObls, openObls) = obls.partition(_.goals.flatMap(_.freevars).toSet.subsetOf(judg.freevars))
//  if (openObls.isEmpty)
//    eqObls ++ closedObls
//  else {
//    val mergedObl: ProofObligation = openObls.reduce((g1, g2) => g1.copy(name = s"${obl.name}-open", goals = g1.goals ++ g2.goals))
//    val freevars = (mergedObl.goals.flatMap(_.freevars)).toSet.diff(judg.freevars)
//    val closedGoals = closedObls.flatMap(_.goals)
//    val existentialMergedObl = mergedObl.copy(
//      existentials = mergedObl.existentials ++ freevars,
//      assumptions = (mergedObl.assumptions ++ closedGoals).distinct,
//      goals = mergedObl.goals.distinct)
//    eqObls ++ closedObls :+ existentialMergedObl
//  }
}