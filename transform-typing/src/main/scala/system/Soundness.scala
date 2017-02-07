package system

import system.Syntax._
import system.Verification._

object Soundness {

  def soundnessTrans(trans: Transformation): Seq[ProofObligation] = {
    val lemmaObls = trans.lemmas.foldLeft[(Seq[ProofObligation], Seq[Rule])](Seq(), Seq()){ case ((obls, lemmas), rule) =>
      (soundnessRule(rule, trans, lemmas, false), lemmas :+ rule._1)
    }
    val contractObls = soundnessRule(trans.contract, trans, trans.lemmas.keys.toSeq, true)
    lemmaObls._1 ++ contractObls
  }

  def soundnessRule(rule: (Rule, Int), trans: Transformation, lemmas: Seq[Rule], isContract: Boolean): Seq[ProofObligation] =
    trans.rewrites.zipWithIndex.map{ case (r, i) => soundnessRewrite(r, i, rule._1, rule._2, trans, lemmas, isContract)(new Gensym) }

  def soundnessRewrite(r: Rewrite, rnum: Int, contract: Rule, contractPos: Int, trans: Transformation, lemmas: Seq[Rule], isContract: Boolean)(implicit gensym: Gensym): ProofObligation = {
    val freshContract = contract.fresh

    freshContract.contractedTerm(contractPos).matchAgainst(r.pat) match {
      case (s, diff, _) if diff.isEmpty =>
        val (ihs, opaques, wellformednessAssumptions) = deriveIHs(r.gen, r, freshContract, contractPos, isContract)
        val sopaques = s.mapValues(_.subst(opaques)) ++ (opaques -- s.keys)

        val name = freshContract.name
        val premises = freshContract.premises.map(_.subst(sopaques))
        val goal = freshContract.conclusion.subst(sopaques).updated(contractPos, r.gen.subst(opaques))
        val where = r.where.map(_.subst(opaques))

        val opaqueSyms = opaques.values.map(_.asInstanceOf[App].sym).toSeq

        val wellformednessRules =
          if (isContract)
            wellformednessAssumptions.zipWithIndex.map { case (wf, i) => Lemma(s"${r.sym}-$rnum-wellformedness-$i", wf)}
          else
            Seq()

        ProofObligation(
          s"${contract.name}-$rnum",
          trans.lang,
          opaqueSyms,
          Set(),
          lemmas ++ wellformednessRules ++ ihs,
          Some(trans),
          premises ++ where,
          goals = Seq(goal),
          gensym)
      case m =>
        throw new MatchError(s"Rewrite rule\n$r\n does not match contract\n${freshContract}\nbecause ${matchDiffMsg(m)}")
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
      case (s, diff, _) if diff.isEmpty =>
        val premises = contract.premises.map(_.subst(s))
        val rule = Rule(contract.name + s"-IH-$num",
          contract.conclusion.updated(contractPos, recApp).subst(s),
          // if ------------
          // no preconditions needed for contracts because we check the wellformedness of transformation calls separately
          if (isContract) List() else premises,
          lemma = true
        )
        Some(rule -> premises)
      case m =>
        print(s"WARNING could not generate IH for recursive call $recApp of $r, because ${matchDiffMsg(m)}")
        None
    }
  }
}