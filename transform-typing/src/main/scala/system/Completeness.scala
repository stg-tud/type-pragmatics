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

    val rewriteConds = trans.rewrites.map(completenessRewrite(_, inputs))
    val goal = mkOr(rewriteConds).asInstanceOf[App].toJudg
    val premises = freshContract.premises


    ProofObligation(
      s"Completeness-${trans.contractedSym}",
      trans.lang,
      Seq(),
      Set(),
      Seq(),
      Some(trans),
      premises,
      goals = Seq(goal),
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
