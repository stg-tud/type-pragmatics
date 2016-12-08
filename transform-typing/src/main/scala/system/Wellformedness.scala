package system

import system.Syntax._
import system.Verification._

import scala.collection.immutable.ListMap

object Wellformedness {

  type FormednessCheck = Seq[Judg]

  def wellformedTrans(trans: Transformation): Seq[ProofObligation] =
    trans.rewrites.zipWithIndex.flatMap{ case (r, i) => wellformedRewrite(r, i, trans)(new Gensym) }

  def wellformedRewrite(r: Rewrite, rnum: Int, trans: Transformation)(implicit gensym: Gensym): Seq[ProofObligation] = {

    val transContracts = trans.contracts.map(kv => kv._1.fresh -> kv._2)
    val otherContracts = trans.lang.transs.map(t => t.contracts.map(kv => kv._1.fresh -> kv._2))
    val checks = wellformedTerm(r.gen, otherContracts :+ transContracts)

    var premises = Seq[Judg]()
    transContracts foreach { case (contract, pos) =>
      contract.contractedTerm(pos).matchAgainst(r.pat) match {
        case Left(s) =>
          premises ++= contract.premises.map(_.subst(s))
          premises ++= r.where.map(kv => Judg(equ(kv._1.sort), kv._1, kv._2).subst(s))
        case Right(msg) =>
          throw new MatchError(s"Rewrite rule\n$r\n does not match contract\n$contract\nbecause $msg")
      }
    }

    checks.zipWithIndex.map { case (check, i) =>
      ProofObligation(s"wf-${trans.contractedSym}-$rnum-$i", trans.lang, Seq(), Seq(), trans, premises, check)
    }
  }

  def wellformedTerm(t: Term, transs: Seq[ListMap[Rule, Int]]): Seq[FormednessCheck] = t match {
    case v: Var => Seq()
    case t@App(sym, kids) =>
      val subs = kids.foldLeft(Seq[FormednessCheck]())((seq, t) => seq ++ wellformedTerm(t, transs))
      transs.find(cs => cs.head._1.contractedTerm(cs.head._2).sym == sym) match {
        case None => subs
        case Some(contracts) =>
          contracts.flatMap(kv => wellformedTransCall(kv._1, kv._2, t)).toSeq ++ subs
      }
  }

  def wellformedTransCall(contract: Rule, pos: Int, app: App): Option[FormednessCheck] = {
    contract.contractedTerm(pos).matchAgainst(app) match {
      case Left(s) =>
        val goals = contract.premises.map(_.subst(s))
        Some(goals)
      case Right(msg) =>
        print(s"WARNING could not generate well-formedness check for recursive call $app")
        None
    }
  }

}