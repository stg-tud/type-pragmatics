package system

import system.Syntax._
import system.Verification._

object Soundness {

  def transSoundness(trans: Transform, lang: Language): Seq[Obligation] =
    trans.rewrites.map(r => rewriteSoundness(r, trans, lang))

  def rewriteSoundness(r: Rewrite, trans: Transform, lang: Language): Obligation =
    trans.contractedTerm.unify(r.pat) match {
      case Left(s) =>
        val name = trans.contract.name
        val premises = trans.contract.premises.map(_.subst(s))
        val assumptions = premises.zipWithIndex.map { case (p, i) => Rule(s"$name-Pre-$i", p) }

        val rhs = r.gen.subst(s)
        val goal = trans.contract.conclusion.updatedCopy(trans.pos, rhs).subst(s)

        val ihs = deriveIHs(rhs, r, trans)

        ProofObligation(lang, assumptions ++ ihs, goals = Seq(goal))
      case Right(msg) =>
        FailedObligation(s"Rewrite rule\n$r\n does not match contract\n${trans.contract}\nbecause $msg")
    }

  def deriveIHs(rhs: Term, r: Rewrite, trans: Transform): Seq[Rule] = {
    val sym = trans.contractedSym
    val recApps = rhs.findAll {
      case App(`sym`, _) => true
      case _ => false
    }

    recApps.zipWithIndex.flatMap {case (recApp, i) => deriveIH(recApp, r, i, trans)}
  }

  // TODO generate opaque symbols to prevent universally applicable IH
  def deriveIH(recApp: Term, r: Rewrite, num: Int, trans: Transform): Option[Rule] = {
    val contract = trans.contract
    trans.contractedTerm.unify(recApp) match {
      case Left(s) =>
        val ihTerm = recApp.subst(s)
        Some(Rule(contract.name + s"-IH-$num",
          contract.conclusion.updatedCopy(trans.pos, ihTerm).subst(s),
          // if -------------
          contract.premises.map(_.subst(s))
        ))
      case Right(msg) =>
        print(s"WARNING could not generate IH for recursive call $recApp of $r")
        None
    }
  }
}