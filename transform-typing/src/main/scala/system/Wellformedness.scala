package system

import system.Syntax._
import system.Verification._

import scala.collection.immutable.ListMap

object Wellformedness {

  type FormednessCheck = Seq[Judg]

  def wellformedTrans(trans: Transformation): Seq[ProofObligation] =
    trans.rewrites.zipWithIndex.flatMap{ case (r, i) => wellformedRewrite(r, i, trans)(new Gensym) }

  def wellformedRewrite(r: Rewrite, rnum: Int, trans: Transformation)(implicit gensym: Gensym): Seq[ProofObligation] = {

    val (contract, pos) = (trans.contract._1.fresh, trans.contract._2)
    val otherContracts = trans.lang.transs.map(t => (t.contract._1.fresh, t.contract._2))
    val checks = wellformedTerm(r.gen, otherContracts :+ (contract, pos))

    val premises = contract.contractedTerm(pos).matchAgainst(r.pat) match {
      case Left(s) =>
        contract.premises.map(_.subst(s)) ++ r.where
      case Right(msg) =>
        throw new MatchError(s"Rewrite rule\n$r\n does not match contract\n$contract\nbecause $msg")
    }
    val premiseVars = premises.flatMap(_.freevars).toSet

    checks.zipWithIndex.map { case (check, i) =>
      val checkVars = check.flatMap(_.freevars).toSet
      val existentials = checkVars.diff(r.boundVars).diff(premiseVars)
      ProofObligation(s"wf-${trans.contractedSym}-$rnum-$i", trans.lang, Seq(), existentials, Seq(), trans, premises, check, gensym)
    }
  }

  def wellformedTerm(t: Term, transs: Seq[(Rule, Int)])(implicit gensym: Gensym): Seq[FormednessCheck] = t match {
    case v: Var => Seq()
    case t@App(sym, kids) =>
      val subs = kids.foldLeft(Seq[FormednessCheck]())((seq, t) => seq ++ wellformedTerm(t, transs))
      transs.find{ case (contract, pos) => contract.contractedTerm(pos).sym == sym } match {
        case None => subs
        case Some((contract, pos)) =>
          wellformedTransCall(contract, pos, t).toSeq ++ subs
      }
  }

  def wellformedTransCall(contract: Rule, pos: Int, app: App)(implicit gensym: Gensym): Option[FormednessCheck] = {
    if (contract.premises.isEmpty)
      return None
    contract.contractedTerm(pos).matchAgainst(app) match {
      case Left(s) =>
        val vars = contract.premises.flatMap(_.freevars).toSet
        val freeVars = vars.diff(s.keys.toSet)
        val freshFreeVars = freeVars.map(v => v -> gensym.freshVar(v.name, v.sort)).toMap
        val s2 = s ++ freshFreeVars
        val goals = contract.premises.map(_.subst(s2))
        Some(goals)
      case Right(msg) =>
        print(s"WARNING could not generate well-formedness check for recursive call $app")
        None
    }
  }

}