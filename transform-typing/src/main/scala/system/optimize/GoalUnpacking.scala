package system.optimize

import system.Syntax._
import system.Verification
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
    obl.goals.flatMap(unpackJudg(_, obl, Map()) match {
      case Proved => Seq()
      case Disproved(steps) => throw new MatchError(s"Obligation was disproved:\n${steps.mkString("\n")}")
      case DontKnow => Seq(obl)
      case ProveThis(obls) => obls
    }).distinct
  }

  private sealed trait Unpacked
  private case object Proved extends Unpacked
  private case object DontKnow extends Unpacked
  private case class Disproved(steps: Seq[(String, Judg)]) extends Unpacked
  private case class ProveThis(obls: Seq[ProofObligation]) extends Unpacked

  private def trySolveEquation(judg: Judg)(implicit counter: Counter): Unpacked =
    if (judg.sym.isEq && judg.terms(0) == judg.terms(1))
    // terms are syntactically equal => equality always holds
      Proved
    else if (judg.sym.isEq && Try(judg.terms(0).unify(judg.terms(1))).isFailure)
    // terms are not unifiable => equality never holds
      Disproved(Seq("#EQ" -> judg))
    else if (judg.sym.isNeq && judg.terms(0) == judg.terms(1))
    // terms are syntactically equal => inequality never holds
      Disproved(Seq("#NEQ" -> judg))
    else if (judg.sym.isNeq && Try(judg.terms(0).unify(judg.terms(1))).isFailure)
    // terms are not unifiable => inequality always holds
      Proved
    else
      DontKnow

  private def unpackJudg(judg: Judg, obl: ProofObligation, cc: CostCount)(implicit counter: Counter): Unpacked = {
    if (obl.assumptions.contains(judg))
      return Proved

    trySolveEquation(judg) match {
      case result@(Proved | Disproved(_) | ProveThis(_)) => return result
      case DontKnow => // continue
    }

    val rules = obl.lang.rules ++ obl.lang.transs.flatMap(_.rules.keys) ++ obl.axioms
    val filteredRules = rules.filter(rule => rule.conclusion.sym == judg.sym && !rule.lemma)
    val candidates = filteredRules.flatMap{ rule =>
      val freshRule = rule.fresh(obl.gensym)
      Try(freshRule -> freshRule.conclusion.matchTerm(judg)).toOption
    }
    val relevantCanditates: Seq[(Rule, Subst, Diff)] = candidates.flatMap{ case (rule, (s, eqs, num)) =>
      // only keep those candidates where some constructors were matched
      val constrMatch = num > 0 || eqs.exists(eq => App.isConstr(eq._2))
      if (!constrMatch)
        None
      else {
        var disproved = false
        val eqGoals = eqs.flatMap { case(t1, t2) =>
          val goal = Judg(equ(t1.sort), t1, t2)
          trySolveEquation(goal) match {
            case Proved => Seq()
            case Disproved(_) => disproved = true; Seq(goal)
            case DontKnow => Seq(goal)
            case ProveThis(obls) => obls.flatMap(_.goals)
          }
        }
        if (disproved)
          None
        else if (eqGoals.isEmpty)
          Some((rule, s, Seq()))
        else {
          val eqObl = obl.copy(name = s"UNPACK-EQ-${obl.name}", goals = eqGoals)
          val eqOblEx = mergeExistentialObligations(judg, Seq(eqObl)).head
          val eqResult = Verification.verify(eqOblEx, timeout = 0.3)
          if (eqResult.status == benchmarking.Proved) {
            // If there were existential variables in the equations, then we only proved satisfiability but not tautology.
            // In such cases we propagate the equations such that we can check together with the rule premises when using this candidate
            val existentialEqs = if (eqOblEx.existentials.isEmpty) Seq() else eqGoals.map(j => (j.terms(0), j.terms(1)))
            Some((rule, s, existentialEqs))
          }
          else
            None
        }
      }
    }

    if (relevantCanditates.size == 0) {
      DontKnow
    }
    else if (relevantCanditates.size == 1) {
      // exactly one candidate rule found, applying it
      val (rule, s, eqs) = relevantCanditates.head
      val eqGoals = eqs.map { case(t1, t2) => Judg(equ(t1.sort), t1, t2) }
      val premiseGoals = rule.premises.map(_.subst(s))
      val goals = eqGoals ++ premiseGoals
      val obls = goals.flatMap { prem => unpackJudg(prem, obl, cc) match {
        case Disproved(steps) =>
          if (relevantCanditates.size == candidates.size)
            return Disproved((rule.name -> judg) +: steps)
          else
            Seq(nextObligation(prem, obl))
        case Proved => Seq()
        case DontKnow => Seq(nextObligation(prem, obl))
        case ProveThis(obls) => obls
      }}
      val mergedObls = mergeExistentialObligations(judg, obls)
      ProveThis(mergedObls)
    }
    else {
      // multiple candidate rules found
      var alternatives = Seq[Unpacked]()
      for ((rule, s, eqs) <- relevantCanditates) {
        if (!canAfford(rule, cc)) {
          // stop looping
          alternatives :+= ProveThis(Seq(nextObligation(judg, obl)))
        }
        else {
          val eqGoals = eqs.map { case (t1, t2) => Judg(equ(t1.sort), t1, t2) }
          val premiseGoals = rule.premises.map(_.subst(s))
          val goals = eqGoals ++ premiseGoals
          var disproved = false
          val obls = goals.flatMap { goal => unpackJudg(goal, obl, using(rule, cc)) match {
            case Disproved(steps) => disproved = true; Seq()
            case Proved => Seq()
            case DontKnow => Seq(nextObligation(goal, obl))
            case ProveThis(obls) => obls
          }}

          val mergedObls = mergeExistentialObligations(judg, obls)
          if (disproved) {
            // ignore this alternative
          }
          else if (mergedObls.isEmpty) {
            // all premises discharged, meaning we just proved `judj`
            return Proved
          }
          else {
            alternatives :+= ProveThis(mergedObls)
          }
        }
      }

      if (alternatives.isEmpty && relevantCanditates.size == candidates.size)
        Disproved(Seq(candidates.map(_._1.name).mkString("Alt(",",",")") -> judg))
      else if (alternatives.size == 1)
        alternatives.head
      else
        ProveThis(Seq(nextObligation(judg, obl)))
    }
  }

  /*
   * Merges those obligations that have overlapping sets of existentially quantified variables resulting from unpacking.
   */
  def mergeExistentialObligations(judg: Judg, goals: Seq[ProofObligation]): Seq[ProofObligation] = {
    var buckets = Seq[(mutable.Set[Var], mutable.ListBuffer[ProofObligation])]()
    val judgVars = judg.freevars

    goals.foreach { g =>
      val vars = mutable.Set[Var]()
      g.goals.foreach(vars ++= _.freevars)
      vars --= (judgVars)
      buckets.find(_._1.exists(v => vars.contains(v))) match {
        case Some((bucketVars, bucket)) =>
          bucketVars ++= vars
          bucket += g
        case None =>
          buckets = buckets :+ (vars, mutable.ListBuffer(g))
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