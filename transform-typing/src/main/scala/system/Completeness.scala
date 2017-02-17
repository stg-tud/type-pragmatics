package system

import system.Syntax._
import system.Verification._


/**
  * Created by seba on 08.02.17.
  */
object Completeness {

  def completenessTrans(trans: Transformation): ProofObligation = {
    implicit val gensym = new Gensym
    val freshContract = trans.contract._1.fresh
    val pos = trans.contract._2
    val inputs = freshContract.contractedTerm(pos).kids

    val premises = freshContract.premises
    val rewriteConds = trans.rewrites.map(completenessRewrite(_, inputs))

    val atLeastOne = mkOr(rewriteConds).asInstanceOf[App].toJudg
    val noOverlap: Seq[Judg] =
      for (i <- 0 until rewriteConds.size;
           j <- i+1 until rewriteConds.size)
        yield OR(NOT(rewriteConds(i)), NOT(rewriteConds(j))).toJudg


    ProofObligation(
      s"Completeness-${trans.contractedSym}",
      trans.lang,
      Seq(),
      Set(),
      Seq(),
      Some(trans),
      premises,
      goals = atLeastOne +: noOverlap,
      gensym)
  }

  def completenessRewrite(rew: Rewrite, inputs: Seq[Term]): Term = {
    val inputMatches = inputs.zip(rew.pat.kids).map{ case (v, t) => Judg(equ(v.sort), v, t) }
    val conds = inputMatches ++ rew.where
    val cond = mkAnd(conds.map(_.toApp))
    val exVars = rew.boundVars
    mkExists(exVars.toSeq, cond)
  }

}
