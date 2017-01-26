package system.optimize

import system.Syntax._
import system.Verification.ProofObligation

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
    obl.goals.flatMap(unpackJudg(_, obl, Map())).distinct
  }

  def trySolveEquation(judg: Judg, obl: ProofObligation)(implicit counter: Counter): Option[Seq[ProofObligation]] =
    if (judg.sym.isEq && judg.terms(0) == judg.terms(1))
    // terms are syntactically equal => equality always holds
      Some(Seq())
    else if (judg.sym.isEq && Try(judg.terms(0).unify(judg.terms(1))).isFailure)
    // terms are not unifiable => equality never holds
      Some(Seq(nextObligation(Judg(FALSE), obl)))
    else if (judg.sym.isNeq && judg.terms(0) == judg.terms(1))
    // terms are syntactically equal => inequality never holds
      Some(Seq(nextObligation(Judg(FALSE), obl)))
    else if (judg.sym.isNeq && Try(judg.terms(0).unify(judg.terms(1))).isFailure)
    // terms are not unifiable => inequality always holds
      Some(Seq())
    else
      None

  def unpackJudg(judg: Judg, obl: ProofObligation, cc: CostCount)(implicit counter: Counter): Seq[ProofObligation] = {
    trySolveEquation(judg, obl).foreach { return _ }

    val rules = obl.lang.rules ++ obl.lang.transs.flatMap(_.rules.keys) ++ obl.axioms
    val filteredRules = rules.filter(rule => rule.conclusion.sym == judg.sym && !rule.lemma)
    val candidates = filteredRules.flatMap{ rule =>
      val freshRule = rule.fresh(obl.gensym)
      val opt = Try(freshRule -> freshRule.conclusion.matchTerm(judg)).toOption
      opt.filter{ case (_, (_, eqs, _)) => !eqs.exists{case (l,r) => App.isFun(l)} }
    }

    if (candidates.size == 0) {
      Seq(nextObligation(judg, obl))
    }
    else if (candidates.size == 1) {
      // exactly one candidate rule found, applying it
      val (rule, (s, eqs, _)) = candidates.head
      val eqGoals = eqs.map { case(t1, t2) => Judg(equ(t1.sort), t1, t2) }
      val eqObls = eqGoals.map(nextObligation(_, obl))
      val premiseGoals = rule.premises.map(_.subst(s))
      val premiseObls = premiseGoals.flatMap(unpackJudg(_, obl, cc))
      val obls = mergeExistentialObligations(judg, eqObls ++ premiseObls)
      obls
    }
    else {
      // multiple candidate rules found
      var alternatives = Seq[Seq[ProofObligation]]()
      for ((rule, (s, eqs, num)) <- candidates) {
        if (eqs.exists{case (l,r) => App.isFun(l) && App.isFun(r)}) {
          // skip this candidate since it requires an eq like `f(x) = g(y)`
        }
        else if (num <= 0 && !eqs.exists(eq => App.isConstr(eq._2))) {
          // no constructors were matched
          alternatives :+= Seq(nextObligation(judg, obl))
        }
        else if (!canAfford(rule, cc)) {
          // stop looping
          alternatives :+= Seq(nextObligation(judg, obl))
        }
        else {
          val eqGoals = eqs.map { case (t1, t2) => Judg(equ(t1.sort), t1, t2) }
          val eqObls = eqGoals.flatMap(unpackJudg(_, obl, cc))
          val premiseGoals = rule.premises.map(_.subst(s))
          val premiseObls = premiseGoals.flatMap(unpackJudg(_, obl, using(rule, cc)))
          val obls = eqObls ++ premiseObls

          if (obls.isEmpty)
          // all premises discharged, meaning we just proved `judj`
            return Seq()
          else if (obls.exists(_.goals.exists(_.sym == FALSE))) {
            // this candidate turned out not useful since we disproved one of its premises
          }
          else {
            alternatives :+= obls
          }
        }
      }

      if (alternatives.isEmpty)
        Seq(nextObligation(Judg(FALSE), obl))
      else if (alternatives.size == 1)
        alternatives.head
      else
        Seq(nextObligation(judg, obl))
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