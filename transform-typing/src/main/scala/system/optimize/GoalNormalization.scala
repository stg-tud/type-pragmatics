package system.optimize

import system.Syntax._
import system.Verification.ProofObligation

import scala.util.Try

/**
  * Normalizes goal equations through
  * - substitution `x = t` or `t = x`
  * - constructor matching `c(x) = c(y)`
  * - transformation expansion `f(t) ~> body`
  */
object GoalNormalization {

  type Eqs = Seq[(Term, Term)]
  type Rewrites = Map[Symbol, Seq[Rewrite]]

  def normalizeObligation(obl: ProofObligation): Seq[ProofObligation] = {
    val (eqGoals, goals) = obl.goals.partition(_.sym.isEq)
    val eqs0 = eqGoals.map(j => j.terms(0) -> j.terms(1))
    val rewrites = Map() ++ obl.allTrans.map(t => t.contractedSym -> t.rewrites)

    val eqs1 = normalizeTransAppsEqs(eqs0)(rewrites)

    var eqsFix: Eqs = eqs1
    var substFix: Subst = Map()
    var done = false
    while (!done) {
      val eqsC = normalizeConstructorEqs(eqsFix)
      val (s, eqsS) = normalizeSubstitutionEqs(eqsC, obl.existentials)
      eqsFix = eqsS
      if (s.isEmpty)
        done = true
      else
        substFix = substFix.mapValues(_.subst(s, true)) ++ s
    }

    val eqGoalsNormed = eqsFix.map{ case (l,r) => Judg(equ(l.sort), l, r).subst(substFix, true) }
    Seq(obl.copy(
      existentials = obl.existentials.diff(substFix.keySet),
      goals = eqGoalsNormed ++ goals.map(_.subst(substFix, true)),
      assumptions = obl.assumptions.map(_.subst(substFix, true)),
      axioms = obl.axioms.map(_.subst(substFix))
    ))
  }

  def normalizeTransAppsEqs(eqs: Eqs)(implicit rewrites: Rewrites): Eqs =
    eqs.flatMap(normalizeTransAppEq(_))

  def normalizeTransAppEq(eq: (Term, Term))(implicit rewrites: Rewrites): Eqs = {
    val (l, r) = eq
    val (lnormed, leqs) = normalizeTransAppTerm(l)
    val (rnormed, reqs) = normalizeTransAppTerm(r)
    (lnormed -> rnormed) +: (leqs ++ reqs)
  }


  def normalizeTransAppTerms(ts: List[Term])(implicit rewrites: Rewrites): (List[Term], Eqs) = {
    var eqs: Eqs = Seq()
    val normed = ts.map{ t =>
      val (newt, newEqs) = normalizeTransAppTerm(t)
      eqs ++= newEqs
      newt
    }
    (normed, eqs)
  }

  def normalizeTransAppTerm(t: Term)(implicit rewrites: Rewrites): (Term, Eqs) = t match {
    case t@App(f, kids) => rewrites.get(f) match {
      case None =>
        val (normedKids, eqs) = normalizeTransAppTerms(kids)
        (App(f, normedKids), eqs)
      case Some(rs) =>
        val (normedKids, eqs) = normalizeTransAppTerms(kids)
        val applicable = applicableRewrites(App(f, normedKids), rs)
        if (applicable.size != 1)
          (App(f, normedKids), eqs)
        else {
          val (tgen, eqs1) = applicable.head
          val (tgenNormed, eqs2) = normalizeTransAppTerm(tgen)
          (tgenNormed, eqs1 ++ eqs2)
        }
    }
    case t@Var(_, _) => (t, Seq())
  }

  def applicableRewrites(t: App, rs: Seq[Rewrite])(implicit rewrites: Rewrites): Seq[(Term, Eqs)] =
    rs.flatMap {
      case r@Rewrite(pat, gen, where) if where.forall(_.sym.isEq) =>
        Try(pat.matchAgainst(t)).toOption match {
          case None => None
          case Some((s, diff, _)) if diff.nonEmpty => None
          case Some((s, _, _)) =>
            val whereEqs = where.map(j => (j.terms(0).subst(s) -> j.terms(1).subst(s)))
            val normedWhereEqs = normalizeTransAppsEqs(whereEqs)
            Some(gen.subst(s), normedWhereEqs)
        }
      case _ => None
    }

  def normalizeConstructorEqs(eqs: Eqs): Eqs =
    eqs.flatMap(normalizeConstructorEq(_))

  def normalizeConstructorEq(eq: (Term, Term)): Eqs = eq match {
    case (l, r) if l == r => Seq()
    case (App(lsym, lkids), App(rsym, rkids)) if lsym == rsym && lsym.constr =>
      normalizeConstructorEqs(lkids zip rkids)
    case _ => Seq(eq)
  }

  def normalizeSubstitutionEqs(eqs: Eqs, existentials: Set[Var]): (Subst, Eqs) = {
    var s: Subst = Map()
    def add(v: Var, t: Term): Option[(Term, Term)] = {
      s.get(v) match {
        case None => s = s.mapValues(_.subst(Map(v -> t), true)) + (v -> t.subst(s, true)); None
        case Some(t0) if t.isInstanceOf[Var] => add(t.asInstanceOf[Var], t0)
        case Some(t0) => Some(t0 -> t)
      }
    }
    val rest = eqs.flatMap {
      case (l@Var(_,_), r@Var(_,_)) if existentials.contains(r) => add(r, l)
      case (l@Var(_, _), r) => add(l, r)
      case (l, r@Var(_, _)) => add(r, l)
      case eq => Some(eq)
    }
    (s, rest.map{case (l, r) => l.subst(s, true) -> r.subst(s, true)})
  }
}
