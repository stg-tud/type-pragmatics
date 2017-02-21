package system

import system.Syntax._
import system.Verification._


/**
  * Created by seba on 08.02.17.
  */
object Completeness {

  def completenessTrans(trans: Transformation): Seq[ProofObligation] = {
    implicit val gensym = new Gensym
    val freshContract = trans.contract._1.fresh
    val pos = trans.contract._2
    val inputs = freshContract.contractedTerm(pos).kids

    val premises = freshContract.premises
    val rewriteConds = trans.rewrites.map(completenessRewrite(_, inputs))

    val matching = mkOr(rewriteConds).asInstanceOf[App].toJudg
    val noOverlap: Seq[(Int, Int, Judg)] =
      for (i <- 0 until rewriteConds.size;
           j <- i+1 until rewriteConds.size)
        yield (i, j, OR(NOT(rewriteConds(i)), NOT(rewriteConds(j))).toJudg)


    val matchingObl =
      ProofObligation(
        s"Matching-${trans.contractedSym}",
        trans.lang,
        Seq(),
        Set(),
        Seq(),
        Some(trans),
        premises,
        goals = Seq(matching),
        gensym)
    val noOverlapObls =
      for ((i, j, goal) <- noOverlap) yield
        ProofObligation(
          s"No-Overlap-${trans.contractedSym}-$i-$j",
          trans.lang,
          Seq(),
          Set(),
          Seq(),
          Some(trans),
          premises,
          goals = Seq(goal),
          gensym)

    matchingObl +: noOverlapObls
  }

  def completenessRewrite(rew: Rewrite, inputs: Seq[Term]): Term = {
    val inputMatches = inputs.zip(rew.pat.kids).map{ case (v, t) => Judg(equ(v.sort), v, t) }
    val conds = inputMatches ++ rew.where
    val cond = mkAnd(conds.map(_.toApp))
    val exVars = rew.boundVars
    mkExists(exVars.toSeq, cond)
  }

}
