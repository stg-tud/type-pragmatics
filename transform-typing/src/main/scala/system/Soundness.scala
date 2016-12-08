package system

import system.Syntax._
import system.Verification._

object Soundness {

  def transSoundness(trans: Transformation): Seq[ProofObligation] =
    trans.contracts.flatMap { case (contract, contractPos) =>
      trans.rewrites.zipWithIndex.map{ case (r, i) => rewriteSoundness(r, i, contract, contractPos, trans)(new Gensym) }
    }.toSeq

  def rewriteSoundness(r: Rewrite, rnum: Int, contract: Rule, contractPos: Int, trans: Transformation)(implicit gensym: Gensym): ProofObligation = {
    val freshContract = contract.fresh

    freshContract.contractedTerm(contractPos).matchAgainst(r.pat) match {
      case Left(s) =>
        val (ihs, opaques) = deriveIHs(r.gen, r, freshContract, contractPos)
        val sopaques = s.mapValues(_.subst(opaques)) ++ (opaques -- s.keys)

        val name = freshContract.name
        val premises = freshContract.premises.map(_.subst(sopaques))
        val goal = freshContract.conclusion.subst(sopaques).updated(contractPos, r.gen.subst(opaques))
        val where = r.where.map(kv => Judg(equ(kv._1.sort), kv._1, kv._2).subst(opaques))

        val opaqueSyms = opaques.values.map(_.asInstanceOf[App].sym).toSeq

        ProofObligation(s"${contract.name}-$rnum", trans.lang, opaqueSyms, ihs, trans, premises ++ where, goals = Seq(goal))
      case Right(msg) =>
        throw new MatchError(s"Rewrite rule\n$r\n does not match contract\n${freshContract}\nbecause $msg")
    }
  }

  def deriveIHs(rhs: Term, r: Rewrite, contract: Rule, contractPos: Int)(implicit gensym: Gensym): (Seq[Rule], Map[Var, Term]) = {
    val sym = contract.contractedTerm(contractPos).sym
    val recApps = rhs.findAll {
      case App(`sym`, _) => true
      case _ => false
    }

    val recVars = recApps.flatMap(_.freevars).toSet
    val opaques = recVars.map(v => v -> App(gensym.freshSymbol(v.name, List(), v.sort))).toMap

    val rules = recApps.zipWithIndex.flatMap {case (recApp, i) =>
      val recAppOpaque = recApp.subst(opaques)
      deriveIH(recAppOpaque, r, i, contract, contractPos)
    }

    (rules, opaques)
  }

  def deriveIH(recApp: Term, r: Rewrite, num: Int, contract: Rule, contractPos: Int)(implicit gensym: Gensym): Option[Rule] = {
    contract.contractedTerm(contractPos).matchAgainst(recApp) match {
      case Left(s) =>
        val rule = Rule(contract.name + s"-IH-$num",
          contract.conclusion.updated(contractPos, recApp).subst(s)
          // if -------------
          // no preconditions because we check the wellformedness of transformation calls separately
        )
        Some(rule)
      case Right(msg) =>
        print(s"WARNING could not generate IH for recursive call $recApp of $r")
        None
    }
  }
}