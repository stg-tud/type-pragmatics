package system.optimize

import system.{Language, Transformation}
import system.Syntax._
import system.Verification.ProofObligation

import scala.collection.mutable

/**
  * Drop rules and functions of symbols that are not reachable from the goal via _calls_ (i.e., via forward reasoning).
  * For example, if foo is not reachable from the goal, drop if even if foo uses bar, which is reachable from the goal.
  */
object DropUnreachableDefinitions {

  type Reach = mutable.Set[Symbol]
  type LangInfo = (Seq[Rule], Map[Symbol, Seq[Rewrite]])

  def dropUnreachable(obl: ProofObligation): ProofObligation = {
    val reach: Reach = mutable.Set()
    val irules = obl.lang.allRules ++ obl.trans.map(t => Seq(t.contract._1) ++ t.lemmas.keys).getOrElse(Seq())
    val irewrites = obl.allTrans.map(t => t.contractedSym -> t.rewrites).toMap
    implicit val linfo = (irules, irewrites)

    reach ++= obl.opaques
    obl.goals.foreach(reachableJudg(_, reach))
    val axioms = obl.axioms.filter{ ax =>
      if (ax.conclusion.symbols.subsetOf(reach)) {
        ax.premises.foreach(reachableJudg(_, reach))
        true
      }
      else false
    }
    val ass = obl.assumptions.filter(_.symbols.subsetOf(reach))
    val trans = obl.trans.filter(t => reach.contains(t.contractedSym))
    val extraSym = obl.trans.flatMap(t => if (trans.isDefined) None else Some(t.contractedSym))
    val lang = dropUnreachable(obl.lang, reach, extraSym)

    ProofObligation(
      obl.name,
      lang,
      obl.opaques,
      obl.existentials,
      axioms,
      trans,
      ass,
      obl.goals,
      obl.gensym
    )
  }

  def dropUnreachable(lang: Language, reach: Reach, extra: Iterable[Symbol]) = {
    val syms = lang.syms.filter(reach.contains(_)) ++ extra
    val rules = lang.rules.filter(_.conclusion.symbols.subsetOf(reach))
    val transs = lang.transs.filter(t => reach.contains(t.contractedSym))
    Language(lang.name, lang.sorts, syms, rules, transs)
  }

  def reachableJudg(judg: Judg, r: Reach)(implicit linfo: LangInfo): Unit = {
    reachableSymbol(judg.sym, r)
    judg.terms.foreach(reachableTerm(_, r))
  }

  def reachableTerm(t: Term, r: Reach)(implicit linfo: LangInfo): Unit = t match {
    case v: Var =>
    case App(sym, kids) =>
      reachableSymbol(sym, r)
      kids.foreach(reachableTerm(_, r))
  }

  def reachableSymbol(sym: Symbol, r: Reach)(implicit linfo: LangInfo): Unit =
    if (!r.contains(sym)) {
      r += sym
      val rules = linfo._1.filter(canReachRule(_, sym))
      rules.foreach { rule =>
        reachableJudg(rule.conclusion, r)
        rule.premises.foreach(reachableJudg(_, r))
      }
      val rewrites = linfo._2.getOrElse(sym, Seq())
      rewrites.foreach { rew =>
        reachableTerm(rew.pat, r)
        reachableTerm(rew.gen, r)
        rew.where.foreach(reachableJudg(_, r))
      }
    }

  def canReachRule(rule: Rule, sym: Symbol): Boolean =
    if (sym.out == Prop && rule.conclusion.sym == sym)
      !rule.lemma
    else if (rule.conclusion.symbols.contains(sym))
      true
    else if (rule.conclusion.sym.isEqNeq)
      rule.premises.exists(_.symbols.contains(sym))
    else
      false
}
