package system

import system.Syntax._
import system.Verification._

object Soundness {

  def transSoundness(trans: Transformation): Seq[Obligation] =
    trans.rewrites.map(r => rewriteSoundness(r, trans)(new Gensym))

  def rewriteSoundness(r: Rewrite, trans: Transformation)(implicit gensym: Gensym): Obligation =
    trans.contractedTerm.matchAgainst(r.pat) match {
      case Left(s) =>
        val (ihs, opaques) = deriveIHs(r.gen, r, trans)
        val sopaques = s.mapValues(_.subst(opaques)) ++ opaques

        val freshContract = trans.contract // TODO fresh contract
        val name = freshContract.name
        val premises = freshContract.premises.map(_.subst(sopaques))
        val goal = freshContract.conclusion.updated(trans.contractPos, r.gen).subst(sopaques)

        val opaqueSyms = opaques.values.map(_.asInstanceOf[App].sym).toSeq

        ProofObligation(trans.lang, opaqueSyms, ihs, trans, premises, goals = Seq(goal))
      case Right(msg) =>
        FailedObligation(s"Rewrite rule\n$r\n does not match contract\n${trans.contract}\nbecause $msg")
    }

  def deriveIHs(rhs: Term, r: Rewrite, trans: Transformation)(implicit gensym: Gensym): (Seq[Rule], Map[Var, Term]) = {
    val sym = trans.contractedSym
    val recApps = rhs.findAll {
      case App(`sym`, _) => true
      case _ => false
    }

    val recVars = recApps.flatMap(_.freevars).toSet
    val opaques = recVars.map(v => v -> App(gensym.freshSymbol(v.name, List(), v.sort))).toMap

    val rules = recApps.zipWithIndex.flatMap {case (recApp, i) =>
      val recAppOpaque = recApp.subst(opaques)
      deriveIH(recAppOpaque, r, i, trans)
    }

    (rules, opaques)
  }

  def deriveIH(recApp: Term, r: Rewrite, num: Int, trans: Transformation)(implicit gensym: Gensym): Option[Rule] = {
    val contract = trans.contract
    trans.contractedTerm.matchAgainst(recApp) match {
      case Left(s) =>
        val rule = Rule(contract.name + s"-IH-$num",
          contract.conclusion.updated(trans.contractPos, recApp).subst(s),
          // if -------------
          contract.premises.map(_.subst(s))
        )
        Some(rule)
      case Right(msg) =>
        print(s"WARNING could not generate IH for recursive call $recApp of $r")
        None
    }
  }
}