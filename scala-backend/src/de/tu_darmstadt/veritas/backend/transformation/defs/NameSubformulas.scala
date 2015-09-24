package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.lowlevel.CollectTypeInfo
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.util.FreeVariables

trait CollectSubformulas extends ModuleTransformation {
  var freshNames = new FreshNames

  //collect the new meta variable names that are generated
  var generatedNames: Set[MetaVar] = Set()

  //collect the additional naming premises that are generated during traversal
  var additionalPremises: Set[TypingRuleJudgment] = Set()

  /**
   * override to control which constructs get named subformulas
   *
   * can use information from path variable, for example
   * parameter vc is the construct itself
   */
  def checkConstruct(vc: VeritasConstruct): Boolean = true

  /**
   * gets direct parent of subformula for which a meta variable is to be generated
   * as parameter
   *
   * this information can be used to construct a certain name
   * default implementation just generates a fresh name with "VAR"
   *
   */
  def newMetaVar(parent: VeritasConstruct): MetaVar = MetaVar(freshNames.freshName("VAR"))

  private def addAdditionalPremise(mv: MetaVar, mexp: FunctionExpMeta): Unit =
    additionalPremises = additionalPremises + FunctionExpJudgment(FunctionExpEq(FunctionMeta(mv), mexp))

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    //the traversal of the other constructs will collect named subformulas as premises
    //and modify variable additionalPremises
    //reset additionalPremises & freshNames for each new typing rule that is traversed
    additionalPremises = Set()
    freshNames = new FreshNames
    generatedNames = Set()
    withSuper(super.transTypingRules(tr)) {
      case t @ TypingRule(n, prems, conss) => Seq(t)
    }
  }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    withSuper(super.transTypingRuleJudgment(trj)) {
      case e @ ExistsJudgment(vl, jl) => {
        val oldaddprems = additionalPremises
        additionalPremises = Set()
        val res = ExistsJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
        additionalPremises = oldaddprems
        res
      }
      case e @ ForallJudgment(vl, jl) => {
        val oldaddprems = additionalPremises
        additionalPremises = Set()
        val res = ForallJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
        additionalPremises = oldaddprems
        res
      }
    }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      case e @ ExistsJudgment(vl, jl) => {
        val oldaddprems = additionalPremises
        additionalPremises = Set()
        val res = ExistsJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
        additionalPremises = oldaddprems
        Seq(res)
      }
      case e @ ForallJudgment(vl, jl) => {
        val oldaddprems = additionalPremises
        additionalPremises = Set()
        val res = ForallJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
        additionalPremises = oldaddprems
        Seq(res)
      }
    }

  protected def findExistingPremise(mexp: FunctionExpMeta): Option[FunctionMeta] = {
    def traversePremises(prems: Seq[TypingRuleJudgment]): Option[FunctionMeta] =
      prems match {
        case Seq() => None
        case p +: ps => p match {
          case FunctionExpJudgment(FunctionExpEq(fm @ FunctionMeta(_), r)) if (r == mexp) => Some(fm)
          case _ => traversePremises(ps)
        }
      }

    traversePremises(additionalPremises.toSeq)
  }

  private def checkNew(mexp: FunctionExpMeta): FunctionExpMeta =
    if (checkConstruct(mexp))
      //check first whether there has already been a premise with the desired mexp
      //if so, don't add a new premise
      findExistingPremise(mexp) match {
        case Some(t) => mexp
        case None => mexp match {
          //only add a new premise if the given mexp is already
          //one of the generated meta variables
          case FunctionMeta(m) if (generatedNames contains m) => mexp
          //in all other cases, add a new premise
          case _ => {
            val newmeta = newMetaVar(path.head)
            generatedNames += newmeta
            addAdditionalPremise(newmeta, mexp)
            mexp
          }
        }
      }
    else mexp

  private def checkNews(mexp: FunctionExpMeta): Seq[FunctionExpMeta] =
    if (checkConstruct(mexp))
      //check first whether there has already been a premise with the desired mexp
      //if so, don't add a new premise
      findExistingPremise(mexp) match {
        case Some(fm) => Seq(mexp)
        case None => mexp match {
          //only add a new premise if the given mexp is already
          //one of the generated meta variables
          case FunctionMeta(m) if (generatedNames contains m) => Seq(mexp)
          //in all other cases, add a new premise
          case _ => {
            val newmeta = newMetaVar(path.head)
            generatedNames += newmeta
            addAdditionalPremise(newmeta, mexp)
            Seq(mexp)
          }
        }
      }
    else Seq(mexp)

  override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta =
    withSuper(super.transFunctionExpMeta(f)) {
      case fmv @ FunctionMeta(mv) => checkNew(fmv)
    }

  override def transFunctionExpMetas(f: FunctionExpMeta): Seq[FunctionExpMeta] =
    withSuper(super.transFunctionExpMetas(f)) {
      case fmv @ FunctionMeta(mv) => checkNews(fmv)
    }

  override def transFunctionExps(f: FunctionExp): Seq[FunctionExp] =
    withSuper(super.transFunctionExps(f)) {
      case fe @ FunctionExpEq(f1, f2)    => Seq(FunctionExpEq(checkNew(f1), checkNew(f2)))
      case fe @ FunctionExpNeq(f1, f2)   => Seq(FunctionExpNeq(checkNew(f1), checkNew(f2)))
      case fe @ FunctionExpIf(c, t, e)   => Seq(FunctionExpIf(checkNew(c), checkNew(t), checkNew(e)))
      case fe @ FunctionExpLet(n, e, i)  => Seq(FunctionExpLet(n, checkNew(e), checkNew(i)))
      case fe @ FunctionExpApp(fn, args) => Seq(FunctionExpApp(fn, args map checkNew))
    }

  override def transFunctionExp(f: FunctionExp): FunctionExp =
    //note: naming of subformulas can anyway only ever be applied within constructs 
    //that accept FunctionExpMeta as arguments
    //(since substitution potentially replaces formula with MetaVar!)
    withSuper(super.transFunctionExp(f)) {
      case fe @ FunctionExpEq(f1, f2)    => FunctionExpEq(checkNew(f1), checkNew(f2))
      case fe @ FunctionExpNeq(f1, f2)   => FunctionExpNeq(checkNew(f1), checkNew(f2))
      case fe @ FunctionExpIf(c, t, e)   => FunctionExpIf(checkNew(c), checkNew(t), checkNew(e))
      case fe @ FunctionExpLet(n, e, i)  => FunctionExpLet(n, checkNew(e), checkNew(i))
      case fe @ FunctionExpApp(fn, args) => FunctionExpApp(fn, args map checkNew)
    }
}

/**
 * introduce variable names for subformulas for which checkConstruct holds
 *
 */
trait NameSubformulas extends ModuleTransformation with CollectSubformulas {

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    withSuper(super.transTypingRules(tr)) {
      case TypingRule(n, prems, conss) => {
        //the traversal of the other constructs will collect named subformulas as premises
        //and modify variable additionalPremises
        Seq(TypingRule(n, additionalPremises.toSeq ++ prems, conss))
      }
    }
  }

  private def makeOrImpl(jl: Seq[TypingRuleJudgment]): Seq[TypingRuleJudgment] = {
    val premisesSeq = (additionalPremises.toSeq map (a => NotJudgment(a))) map (s => Seq(s))
    Seq(OrJudgment(premisesSeq :+ jl))
  }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    withSuper(super.transTypingRuleJudgment(trj)) {
      case e @ ExistsJudgment(vl, jl) => {
        val previousFV = FreeVariables.freeVariables(Seq(e))
        val newexistsbody = makeOrImpl(trace(jl)(transTypingRuleJudgments(_)))
        ExistsJudgment((FreeVariables.freeVariables(newexistsbody, previousFV)).toSeq, newexistsbody)
      }
      case e @ ForallJudgment(vl, jl) => {
        val previousFV = FreeVariables.freeVariables(Seq(e))
        val newforallbody = makeOrImpl(trace(jl)(transTypingRuleJudgments(_)))
        ForallJudgment((FreeVariables.freeVariables(newforallbody, previousFV)).toSeq, newforallbody)
      }
    }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      case e @ ExistsJudgment(vl, jl) => {
        val previousFV = FreeVariables.freeVariables(Seq(e))
        val newexistsbody = makeOrImpl(trace(jl)(transTypingRuleJudgments(_)))
        Seq(ExistsJudgment((FreeVariables.freeVariables(newexistsbody, previousFV)).toSeq, newexistsbody))
      }
      case e @ ForallJudgment(vl, jl) => {
        val previousFV = FreeVariables.freeVariables(Seq(e))
        val newforallbody = makeOrImpl(trace(jl)(transTypingRuleJudgments(_)))
        Seq(ForallJudgment((FreeVariables.freeVariables(newforallbody, previousFV)).toSeq, newforallbody))
      }
    }

  private def checkAndSubstituteMexp(mexp: FunctionExpMeta): FunctionExpMeta =
    //check whether there has already been a premise with the desired mexp
    findExistingPremise(mexp) match {
      case Some(fm) => fm
      case None     => mexp
    }

  private def checkAndSubstituteMexps(mexp: FunctionExpMeta): Seq[FunctionExpMeta] =
    //check whether there has already been a premise with the desired mexp
    findExistingPremise(mexp) match {
      case Some(fm) => Seq(fm)
      case None     => Seq(mexp)
    }

  override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta =
    withSuper(super.transFunctionExpMeta(f)) {
      case fmv @ FunctionMeta(mv) => checkAndSubstituteMexp(fmv)
    }

  override def transFunctionExpMetas(f: FunctionExpMeta): Seq[FunctionExpMeta] =
    withSuper(super.transFunctionExpMetas(f)) {
      case fmv @ FunctionMeta(mv) => checkAndSubstituteMexps(fmv)
    }

  override def transFunctionExps(f: FunctionExp): Seq[FunctionExp] =
    withSuper(super.transFunctionExps(f)) {
      case fe @ FunctionExpEq(f1, f2)    => Seq(FunctionExpEq(checkAndSubstituteMexp(f1), checkAndSubstituteMexp(f2)))
      case fe @ FunctionExpNeq(f1, f2)   => Seq(FunctionExpNeq(checkAndSubstituteMexp(f1), checkAndSubstituteMexp(f2)))
      case fe @ FunctionExpIf(c, t, e)   => Seq(FunctionExpIf(checkAndSubstituteMexp(c), checkAndSubstituteMexp(t), checkAndSubstituteMexp(e)))
      case fe @ FunctionExpLet(n, e, i)  => Seq(FunctionExpLet(n, checkAndSubstituteMexp(e), checkAndSubstituteMexp(i)))
      case fe @ FunctionExpApp(fn, args) => Seq(FunctionExpApp(fn, args map checkAndSubstituteMexp))
    }

  override def transFunctionExp(f: FunctionExp): FunctionExp =
    //note: naming of subformulas can anyway only ever be applied within constructs 
    //that accept FunctionExpMeta as arguments
    //(since substitution potentially replaces formula with MetaVar!)
    withSuper(super.transFunctionExp(f)) {
      case fe @ FunctionExpEq(f1, f2)    => FunctionExpEq(checkAndSubstituteMexp(f1), checkAndSubstituteMexp(f2))
      case fe @ FunctionExpNeq(f1, f2)   => FunctionExpNeq(checkAndSubstituteMexp(f1), checkAndSubstituteMexp(f2))
      case fe @ FunctionExpIf(c, t, e)   => FunctionExpIf(checkAndSubstituteMexp(c), checkAndSubstituteMexp(t), checkAndSubstituteMexp(e))
      case fe @ FunctionExpLet(n, e, i)  => FunctionExpLet(n, checkAndSubstituteMexp(e), checkAndSubstituteMexp(i))
      case fe @ FunctionExpApp(fn, args) => FunctionExpApp(fn, args map checkAndSubstituteMexp)
    }

}

object NameEverything extends NameSubformulas

/**
 * excludes all meta variables from being named
 */
object NameEverythingButMetaVars extends NameSubformulas {
  override def checkConstruct(vc: VeritasConstruct): Boolean =
    vc match {
      case FunctionMeta(_) => false
      case MetaVar(_)      => false
      case _               => true
    }
}

/**
 * names for non-boolean function results only (RESULT-variable)
 */
object NameFunctionResultsOnly extends NameSubformulas {
  override def checkConstruct(vc: VeritasConstruct): Boolean = {
    //only rename right-hand side of single equation in conclusion
    val grandgrandparent = path(2)

    grandgrandparent match {
      case TypingRule(n, prems, Seq(FunctionExpJudgment(FunctionExpEq(l, r)))) if (r == vc) => true
      case _ => false
    }
  }

  override def newMetaVar(parent: VeritasConstruct): MetaVar = MetaVar("RESULT")

}




