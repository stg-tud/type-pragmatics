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

    val rewritePatConds = trans.rewrites.map(completenessRewritePats(_, inputs)).distinct
    val matchingPats = mkOr(rewritePatConds).asInstanceOf[App].toJudg
    val matchingPatsObl =
      ProofObligation(
        s"Matching-${trans.contractedSym}-Pats",
        trans.lang,
        Seq(),
        Set(),
        Seq(),
        Some(trans),
        premises,
        goals = Seq(matchingPats),
        gensym)

    val rewriteConds = trans.rewrites.map(completenessRewrite(_, inputs))
    val matching = mkOr(rewriteConds).asInstanceOf[App].toJudg
    val rewritePatJudgs = trans.rewrites.map(completenessRewritePatsJudg(_, inputs)).distinct
    val matchingObls = rewritePatJudgs.zipWithIndex.map { case (patJudg, i) =>
      ProofObligation(
        s"Matching-${trans.contractedSym}-$i",
        trans.lang,
        Seq(),
        Set(),
        Seq(),
        Some(trans),
        patJudg +: premises,
        goals = Seq(matching),
        gensym)
    }

    val rewriteOverlapConds = trans.rewrites.map(completenessRewrite(_, inputs))
    val noOverlap: Seq[(Int, Int, Judg)] =
      for (i <- 0 until rewriteOverlapConds.size;
           j <- i+1 until rewriteOverlapConds.size)
        yield (i, j, OR(NOT(rewriteOverlapConds(i)), NOT(rewriteOverlapConds(j))).toJudg)
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

    matchingPatsObl +: (matchingObls ++ noOverlapObls)
  }

  def completenessRewrite(rew: Rewrite, inputs: Seq[Term]): Term = {
    val inputMatches = inputs.zip(rew.pat.kids).map{ case (v, t) => Judg(equ(v.sort), v, t) }
    val conds = inputMatches ++ rew.where
    val cond = mkAnd(conds.map(_.toApp))
    val exVars = rew.boundVars
    mkExists(exVars.toSeq, cond)
  }

  def completenessRewritePats(rew: Rewrite, inputs: Seq[Term]): Term = {
    val inputMatches = inputs.zip(rew.pat.kids).map{ case (v, t) => Judg(equ(v.sort), v, t) }
    val conds = inputMatches
    val cond = mkAnd(conds.map(_.toApp))
    val exVars = rew.pat.freevars
    mkExists(exVars.toSeq, cond)
  }

  def completenessRewritePatsJudg(rew: Rewrite, inputs: Seq[Term]): Judg = {
    val inputMatches = inputs.zip(rew.pat.kids).map{ case (v, t) => Judg(equ(v.sort), v, t) }
    val conds = inputMatches
    val cond = mkAnd(conds.map(_.toApp))
    cond.asInstanceOf[App].toJudg
  }

}
