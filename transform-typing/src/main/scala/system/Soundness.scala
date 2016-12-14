package system

import system.Syntax._
import system.Verification._

object Soundness {

  def transSoundness(trans: Transformation): Seq[ProofObligation] =
    transRule(trans.contract, trans, true) ++ trans.lemmas.flatMap(transRule(_, trans, false))

  def transRule(rule: (Rule, Int), trans: Transformation, isContract: Boolean): Seq[ProofObligation] =
    trans.rewrites.zipWithIndex.map{ case (r, i) => rewriteSoundness(r, i, rule._1, rule._2, trans, isContract)(new Gensym) }

  def rewriteSoundness(r: Rewrite, rnum: Int, contract: Rule, contractPos: Int, trans: Transformation, isContract: Boolean)(implicit gensym: Gensym): ProofObligation = {
    val freshContract = contract.fresh

    freshContract.contractedTerm(contractPos).matchAgainst(r.pat) match {
      case Left(s) =>
        val (ihs, opaques, wellformednessAssumptions) = deriveIHs(r.gen, r, freshContract, contractPos, isContract)
        val sopaques = s.mapValues(_.subst(opaques)) ++ (opaques -- s.keys)

        val name = freshContract.name
        val premises = freshContract.premises.map(_.subst(sopaques))
        val goal = freshContract.conclusion.subst(sopaques).updated(contractPos, r.gen.subst(opaques))
        val where = r.where.map(_.subst(opaques))

        val opaqueSyms = opaques.values.map(_.asInstanceOf[App].sym).toSeq

        val wellformednessRules =
          if (isContract)
            wellformednessAssumptions.zipWithIndex.map { case (wf, i) => Rule(s"${r.sym}-$rnum-wellformedness-$i", wf)}
          else
            Seq()

        ProofObligation(
          s"${contract.name}-$rnum",
          trans.lang,
          opaqueSyms,
          Set(),
          wellformednessRules ++ ihs,
          trans,
          premises ++ where,
          goals = Seq(goal),
          gensym)
      case Right(msg) =>
        throw new MatchError(s"Rewrite rule\n$r\n does not match contract\n${freshContract}\nbecause $msg")
    }
  }

  def deriveIHs(rhs: Term, r: Rewrite, contract: Rule, contractPos: Int, isContract: Boolean)(implicit gensym: Gensym): (Seq[Rule], Map[Var, Term], Seq[Judg]) = {
    val sym = contract.contractedTerm(contractPos).sym
    val recApps = rhs.findAll {
      case App(`sym`, _) => true
      case _ => false
    }

    val recVars = recApps.flatMap(_.freevars).toSet
    val opaques = recVars.map(v => v -> App(gensym.freshSymbol(v.name, List(), v.sort))).toMap

    val rulesWellformedness = recApps.zipWithIndex.flatMap {case (recApp, i) =>
      val recAppOpaque = recApp.subst(opaques)
      deriveIH(recAppOpaque, r, i, contract, contractPos, isContract)
    }

    val (rules, wellformednessAssumptions) = rulesWellformedness.unzip

    (rules, opaques, wellformednessAssumptions.flatten.distinct)
  }

  def deriveIH(recApp: Term, r: Rewrite, num: Int, contract: Rule, contractPos: Int, isContract: Boolean)(implicit gensym: Gensym): Option[(Rule, Seq[Judg])] = {
    contract.contractedTerm(contractPos).matchAgainst(recApp) match {
      case Left(s) =>
        val premises = contract.premises.map(_.subst(s))
        val rule = Rule(contract.name + s"-IH-$num",
          contract.conclusion.updated(contractPos, recApp).subst(s),
          // if ------------
          // no preconditions needed for contracts because we check the wellformedness of transformation calls separately
          if (isContract) List() else premises,
          lemma = true
        )
        Some(rule -> premises)
      case Right(msg) =>
        print(s"WARNING could not generate IH for recursive call $recApp of $r")
        None
    }
  }
}