package system.optimize

import system.{Language, Transformation}
import system.Syntax._
import system.Verification.ProofObligation

import scala.collection.mutable

/**
  * Drop rules and functions of symbols that are not reachable from the goal via _calls_ (i.e., via forward reasoning).
  * For example, if foo is not reachable from the goal, drop it even if foo uses bar, which is reachable from the goal.
  */
object DropUnreachableDefinitions {

  type Reach = mutable.Set[Symbol]
  type LangInfo = (Seq[Rule], Map[Symbol, Seq[Rewrite]])

  def allReachable(syms: Set[Symbol], reach: Reach): Boolean = syms.forall(reachable(_, reach))

  def reachable(sym: Symbol, reach: Reach): Boolean = sym.isEqNeq || reach.contains(sym)

  def dropUnreachable(obl: ProofObligation): ProofObligation = {
    val reach: Reach = mutable.Set()
    val irules = obl.lang.allRules ++ obl.trans.map(t => Seq(t.contract._1) ++ t.lemmas.keys).getOrElse(Seq())
    val irewrites = obl.allTrans.map(t => t.contractedSym -> t.rewrites).toMap
    implicit val linfo = (irules, irewrites)

    reach ++= obl.opaques
    obl.goals.foreach(reachJudg(_, reach))

    val axioms = obl.axioms.filter{ ax =>
      if (allReachable(ax.symbols, reach)) {
        ax.premises.foreach(reachJudg(_, reach))
        true
      }
      else false
    }
    val ass = obl.assumptions.filter(a => allReachable(a.symbols, reach))
    val trans = obl.trans.map(t =>
      if (reachable(t.contractedSym, reach)) t
      else new DummyTrans(t))
    val lang = dropUnreachable(obl.lang, reach)

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

  class DummyTrans(t: Transformation) extends Transformation(null) {
    override val contract: (Rule, Int) = (Rule("Dummy", Judg(TRUE)), -1)
    override val rewrites: Seq[Rewrite] = Seq()
    override lazy val contractedSym: Symbol = t.contractedSym
    override lazy val undeclaredSymbols: Set[Symbol] = t.undeclaredSymbols
  }

  def dropUnreachable(lang: Language, reach: Reach): Language = {
    val syms = lang.syms.filter(reachable(_, reach))
    val rules = lang.rules.filter(r => allReachable(r.symbols, reach))
    val transs = lang.transs.filter(t => reachable(t.contractedSym, reach))
    Language(lang.name, lang.sorts, syms, rules, transs)
  }

  def reachJudg(judg: Judg, r: Reach)(implicit linfo: LangInfo): Unit = {
    reachSymbol(judg.sym, r)
    judg.terms.foreach(reachTerm(_, r))
  }

  def reachTerm(t: Term, r: Reach)(implicit linfo: LangInfo): Unit = t match {
    case v: Var =>
    case App(sym, kids) =>
      reachSymbol(sym, r)
      kids.foreach(reachTerm(_, r))
  }

  def reachSymbol(sym: Symbol, r: Reach)(implicit linfo: LangInfo): Unit =
    if (!r.contains(sym)) {
      r += sym
      val rules = linfo._1.filter(canReachRule(_, sym))
      rules.foreach { rule =>
        reachJudg(rule.conclusion, r)
        rule.premises.foreach(reachJudg(_, r))
      }
      val rewrites = linfo._2.getOrElse(sym, Seq())
      rewrites.foreach { rew =>
        reachTerm(rew.pat, r)
        reachTerm(rew.gen, r)
        rew.where.foreach(reachJudg(_, r))
      }
    }

  def canReachRule(rule: Rule, sym: Symbol): Boolean =
    if (sym.out == Prop && rule.conclusion.sym == sym)
      !rule.isLemma
    else if (rule.conclusion.symbols.contains(sym))
      true
    else if (sym.isEqNeq || rule.conclusion.sym.isEqNeq)
      rule.premises.exists(_.symbols.contains(sym))
    else
      false
}