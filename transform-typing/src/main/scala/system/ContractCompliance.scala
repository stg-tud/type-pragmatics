package system

import system.Syntax._
import system.Verification._

import scala.collection.immutable.ListMap

/*
 * Wellformedness checks ensure that calls to transformations only occur with arguments satisfying the contract.
 */
object ContractCompliance {

  type FormednessCheck = (Symbol, Seq[Judg])

  def complianceTrans(trans: Transformation): Seq[ProofObligation] = {
    implicit val gensym = new Gensym
    val otherContracts = trans.lang.transs.map(t => t.contractedSym -> (t.contract._1.fresh, t.contract._2)).toMap
    val (contract, pos) = (trans.contract._1.fresh, trans.contract._2)
    val contracts = otherContracts + (trans.contractedSym -> (contract, pos))
    val wfRewrites = trans.rewrites.zipWithIndex.flatMap { case (r, i) => complianceRewrite(r, i, contract, pos, contracts, trans) }
    val wfContract = complianceRule(trans.contract._1, 0, Some(trans.contractedSym), contracts, trans)
    val wfLemmas = trans.lemmas.zipWithIndex.flatMap { case (lem, ix) =>
      complianceRule(lem._1, ix+1, None, contracts, trans)
    }
    wfRewrites ++ wfContract ++ wfLemmas
  }


  /*
   * Transformation calls in the rules conclusion and premises must be well-formed given the rule's premises
   */
  def complianceRule(r: Rule, rnum: Int, skipSymbol: Option[Symbol], contracts: Map[Symbol, (Rule, Int)], trans: Transformation)(implicit gensym: Gensym): Seq[ProofObligation] = {
    val checks = complianceJudg(r.conclusion, contracts) ++ r.premises.flatMap(complianceJudg(_, contracts))

    val ruleVars = r.conclusion.freevars ++ r.premises.flatMap(_.freevars)

    for (symChecks <- checks.groupBy(_._1).values.toSeq;
         ((sym, check), i) <- symChecks.zipWithIndex if !skipSymbol.isDefined || skipSymbol.get != sym) yield {
      val checkVars = check.flatMap(_.freevars).toSet
      val existentials = checkVars.diff(ruleVars)
      ProofObligation(s"Contract-Compliance-${trans.contractedSym}-rule-$rnum-$sym-$i", trans.lang, Seq(), existentials, Seq(), Some(trans), r.premises, check, gensym)
    }
  }

  /*
   * Transformation calls in the rewrite template have to be well-formed assuming the rewrite's contract holds
   */
  def complianceRewrite(r: Rewrite, rnum: Int, contract: Rule, pos: Int, contracts: Map[Symbol, (Rule, Int)], trans: Transformation)(implicit gensym: Gensym): Seq[ProofObligation] = {
    val checks = complianceTerm(r.gen, contracts) ++ r.where.flatMap(complianceJudg(_, contracts))

    val (conclusion, premises) = contract.contractedTerm(pos).matchAgainst(r.pat) match {
      case (s, diff, _) if diff.isEmpty =>
        (contract.conclusion.subst(s), contract.premises.map(_.subst(s)) ++ r.where)
      case m =>
        throw new MatchError(s"Rewrite rule\n$r\n does not match contract\n$contract\nbecause ${matchDiffMsg(m)}")
    }
    val contractVars = conclusion.freevars ++ premises.flatMap(_.freevars)

    for (symChecks <- checks.groupBy(_._1).values.toSeq;
         ((sym, check), i) <- symChecks.zipWithIndex) yield {
      val checkVars = check.flatMap(_.freevars).toSet
      val existentials = checkVars.diff(r.boundVars).diff(contractVars)
      ProofObligation(s"Contract-Compliance-${trans.contractedSym}-rewrite-$rnum-$sym-$i", trans.lang, Seq(), existentials, Seq(), Some(trans), premises, check, gensym)
    }
  }

  def complianceJudg(j: Judg, contracts: Map[Symbol, (Rule, Int)])(implicit gensym: Gensym): Seq[FormednessCheck] =
    j.terms.foldLeft(Seq[FormednessCheck]())((seq, t) => seq ++ complianceTerm(t, contracts))


  def complianceTerm(t: Term, contracts: Map[Symbol, (Rule, Int)])(implicit gensym: Gensym): Seq[FormednessCheck] = t match {
    case v: Var => Seq()
    case t@App(sym, kids) =>
      val subs = kids.foldLeft(Seq[FormednessCheck]())((seq, t) => seq ++ complianceTerm(t, contracts))
      contracts.get(sym) match {
        case None => subs
        case Some((contract, pos)) =>
          complianceTransCall(contract, pos, t).toSeq ++ subs
      }
  }

  def complianceTransCall(contract: Rule, pos: Int, app: App)(implicit gensym: Gensym): Option[FormednessCheck] = {
    if (contract.premises.isEmpty)
      return None
    contract.contractedTerm(pos).matchAgainst(app) match {
      case (s, diff, _) if diff.isEmpty =>
        val vars = contract.premises.flatMap(_.freevars).toSet
        val freeVars = vars.diff(s.keys.toSet)
        val freshFreeVars = freeVars.map(v => v -> gensym.freshVar(v.name, v.sort)).toMap
        val s2 = s ++ freshFreeVars
        val goals = contract.premises.map(_.subst(s2))
        Some((contract.contractedTerm(pos).sym, goals))
      case m =>
        print(s"WARNING could not generate well-formedness check for recursive call $app because ${matchDiffMsg(m)}")
        None
    }
  }

}