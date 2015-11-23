package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.lowlevel.CollectTypeInfo
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.util.FreeVariables

/*
 * trait for collecting equations that can be inlined into other formulas
 */
trait CollectInlineEquations extends ModuleTransformation {

  //collect equations chosen for inlining (left side replaced by right side)
  var chosenSubstitutions: Map[MetaVar, FunctionExpMeta] = Map()
  var freshNames = new FreshNames

  //collect premises that could be premises of an implication
  //represented with an or
  var orImplSubstitutions: Map[MetaVar, FunctionExpMeta] = Map()

  /**
   * override to control which equations can be chosen for being inlined
   *
   * can use information from path variable, for example
   * parameter vc is the construct itself
   */
  def checkConstruct(vc: VeritasConstruct): Boolean = true

  override def apply(m: Seq[Module]): Seq[Module] = {
    //make sure that any mutable state is initialized upon application!
    chosenSubstitutions = Map()
    orImplSubstitutions = Map()
    freshNames = new FreshNames
    super.apply(m)
  }

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    //the traversal of the other constructs will collect named subformulas as premises
    //and modify variable chosenSubstitutions
    //reset chosenSubstitutions & freshNames for each new typing rule that is traversed
    chosenSubstitutions = Map()
    orImplSubstitutions = Map()
    freshNames = new FreshNames
    tr match {
      case t @ TypingRule(n, prems, conss) => super.transTypingRules(t)
    }
  }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    trj match {
      case f @ FunctionExpJudgment(eq @ FunctionExpEq(FunctionMeta(m), r)) => {
        if (checkConstruct(eq) && !chosenSubstitutions.isDefinedAt(m))
          chosenSubstitutions = chosenSubstitutions + (m -> r)
        //will always contain the binding for the first (!) instance of a meta variable m within a premise list
        FunctionExpJudgment(trace(eq)(transFunctionExp(_)))
      }
      case e @ ExistsJudgment(vl, jl) => {
        val oldchosen = chosenSubstitutions
        //add substitutions for quantified variables with fresh names
        //(capture-avoidance, simply rename all quantified variables in body)
        vl map { m => chosenSubstitutions + (m -> FunctionMeta(MetaVar(freshNames.freshName(m.name)))) }
        val res = ExistsJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
        chosenSubstitutions = oldchosen
        res
      }
      case e @ ForallJudgment(vl, jl) => {
        val oldchosen = chosenSubstitutions
        //add substitutions for quantified variables with fresh names
        //(capture-avoidance, simply rename all quantified variables in body)
        vl map { m => chosenSubstitutions + (m -> FunctionMeta(MetaVar(freshNames.freshName(m.name)))) }
        val res = ForallJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
        chosenSubstitutions = oldchosen
        res
      }
      case OrJudgment(orc) => {
        //not sure if this works for nested ors...
        // reset for each new Or
        orImplSubstitutions = Map()
        val res = OrJudgment(orc map (sor => {
          val oldchosen = chosenSubstitutions
          val res = trace(sor)(transTypingRuleJudgments(_))
          chosenSubstitutions = oldchosen
          res
        }))
        res
      }
      case NotJudgment(jdg) => {
        val oldchosen = chosenSubstitutions
        val res = NotJudgment(trace(jdg)(transTypingRuleJudgment(_)))
        //add substitutions that were found in not judgments
        //possibly overwriting substitutions from before...!
        //(premises of an implication)
        orImplSubstitutions = chosenSubstitutions ++ orImplSubstitutions
        chosenSubstitutions = oldchosen
        res
      }
      case t => super.transTypingRuleJudgment(t)
    }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    trj match {
      case f @ FunctionExpJudgment(eq @ FunctionExpEq(FunctionMeta(m), r)) => {
        if (checkConstruct(eq) && !chosenSubstitutions.isDefinedAt(m))
          chosenSubstitutions = chosenSubstitutions + (m -> r)
        //will always contain the binding for the first (!) instance of a meta variable m within a premise list
        Seq(FunctionExpJudgment(trace(eq)(transFunctionExp(_))))
      }
      case e @ ExistsJudgment(vl, jl) => {
        val oldchosen = chosenSubstitutions
        //add substitutions for quantified variables with fresh names
        vl map { m => chosenSubstitutions + (m -> FunctionMeta(MetaVar(freshNames.freshName(m.name)))) }
        val res = ExistsJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
        chosenSubstitutions = oldchosen
        Seq(res)
      }
      case e @ ForallJudgment(vl, jl) => {
        val oldchosen = chosenSubstitutions
        //add substitutions for quantified variables with fresh names
        vl map { m => chosenSubstitutions + (m -> FunctionMeta(MetaVar(freshNames.freshName(m.name)))) }
        val res = ForallJudgment(trace(vl)(transMetaVars(_)), trace(jl)(transTypingRuleJudgments(_)))
        chosenSubstitutions = oldchosen
        Seq(res)
      }
      case OrJudgment(orc) => {
        //not sure if this works for nested ors...
        orImplSubstitutions = Map()
        val res = OrJudgment(orc map (sor => {
          val oldchosen = chosenSubstitutions
          val res = trace(sor)(transTypingRuleJudgments(_))
          orImplSubstitutions = chosenSubstitutions ++ orImplSubstitutions
          chosenSubstitutions = oldchosen
          res
        }))
        Seq(res)
      }
      case NotJudgment(jdg) => {
        val oldchosen = chosenSubstitutions
        val res = NotJudgment(trace(jdg)(transTypingRuleJudgment(_)))
        //add substitutions that were found in not judgments
        //possibly overwriting substitutions from before...!
        //(premises of an implication)
        orImplSubstitutions = chosenSubstitutions ++ orImplSubstitutions
        chosenSubstitutions = oldchosen
        Seq(res)
      }
      case t => super.transTypingRuleJudgments(t)
    }

}

trait InlineSubformulas extends ModuleTransformation with CollectInlineEquations {
  /**
   * save which variables were used for substitution in a single typing rule
   *
   */
  var substitutedVars: Set[String] = Set()

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    //reset state for each typing rule that is traversed
    substitutedVars = Set()
    tr match {
      case t @ TypingRule(n, prems, conss) => super.transTypingRules(t)
    }
  }

  /**
   * override to control which constructs actually get substituted
   * be careful when overriding to not destroy capture-avoidance
   * and not to substitute in the inline equations themselves
   *
   * can use information from path variable, for example
   * parameter vc is the construct itself
   *
   */
  def checkSubstitute(vc: VeritasConstruct): Boolean = {
    //exclude meta variables on RHS of equations of the form m = ... m ....
    val relparent = if (vc.isInstanceOf[MetaVar] && path.isDefinedAt(2)) path(2) else path(1)
    relparent match {
      case FunctionExpEq(FunctionMeta(m), r) if (chosenSubstitutions.isDefinedAt(m) && chosenSubstitutions(m) == r) => false
      case _ => true
    }
  }

  // inside ors, apply substitution that is given by orImplSubstitutions!
  // this ensures that substitution within Ors that represent implications works as desired
  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    withSuper(super.transTypingRuleJudgment(trj)) {
      case OrJudgment(orc) => {
        val oldchosen = chosenSubstitutions
        chosenSubstitutions = orImplSubstitutions
        val substitutedCases = orc map (s => s flatMap transTypingRuleJudgments)
        chosenSubstitutions = oldchosen
        OrJudgment(substitutedCases)
      }
    }

  // inside ors, apply substitution that is given by orImplSubstitutions!
  // this ensures that substitution within Ors that represent implications works as desired
  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      case OrJudgment(orc) => {
        val oldchosen = chosenSubstitutions
        chosenSubstitutions = orImplSubstitutions
        val substitutedCases = orc map (s => s flatMap transTypingRuleJudgments)
        chosenSubstitutions = oldchosen
        Seq(OrJudgment(substitutedCases))
      }
    }

  //substitute meta vars with meta vars in quantified varlists
  //add variables that were used to substitutedVars
  override def transMetaVar(m: MetaVar): MetaVar =
    if (chosenSubstitutions.isDefinedAt(m) && checkSubstitute(m))
      chosenSubstitutions(m) match {
        case FunctionMeta(n) => { substitutedVars = substitutedVars + m.name; n }
        case _               => m
      }
    else m

  override def transMetaVars(m: MetaVar): Seq[MetaVar] =
    if (chosenSubstitutions.isDefinedAt(m) && checkSubstitute(m))
      chosenSubstitutions(m) match {
        case FunctionMeta(n) => { substitutedVars = substitutedVars + m.name; Seq(n) }
        case _               => Seq(m)
      }
    else Seq(m)

  //in FunctionExpMeta, substitute meta vars with what is given in chosenSubstitution
  //add variables that were used to substitutedVars
  override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta =
    withSuper(super.transFunctionExpMeta(f)) {
      case fm @ FunctionMeta(m) if (checkSubstitute(fm) && chosenSubstitutions.isDefinedAt(m)) =>
        { substitutedVars = substitutedVars + m.name; transFunctionExpMeta(chosenSubstitutions(m)) } //substitute in substitution, if necessary
    }

  override def transFunctionExpMetas(f: FunctionExpMeta): Seq[FunctionExpMeta] =
    withSuper(super.transFunctionExpMetas(f)) {
      case fm @ FunctionMeta(m) if (checkSubstitute(fm) && chosenSubstitutions.isDefinedAt(m)) =>
        { substitutedVars = substitutedVars + m.name; transFunctionExpMetas(chosenSubstitutions(m)) }
    }

}

/**
 * trait to control which equations used for substitution
 * will be removed from the a typing rule
 *
 * takes as parameter a set with variables that were substituted during a previous transformation
 */
class RemoveUnusedPremises(substitutedVars: Set[String]) extends ModuleTransformation {
  /**
   * override to control which of the equations marked for inlining
   * get actually removed from the premises
   *
   * default: remove all premises that were used at least once during substitution
   * (but leave premises that were never used!)
   */
  def removeNamingPremise(feq: FunctionExpEq): Boolean =
    feq match {
      case FunctionExpEq(FunctionMeta(m), r) if (substitutedVars contains m.name) => true
      case _ => false
    }

  //no overriding of def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment:
  //cannot remove a single equation by itself (can happen for example inside Not-Judgments, should not just be removed)

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      case f @ FunctionExpJudgment(eq @ FunctionExpEq(_, _)) if (removeNamingPremise(eq)) => Seq()
      //below, naming premises from bodies already have been removed via super call and recursion
      //also, substitutions have already been performed in bodies and variable lists!
      case e @ ExistsJudgment(vl, jl) => {
        if (jl.isEmpty)
          Seq() //remove construct entirely if body became empty
        else {
          //keep only the quantified variables that are still in the body
          //there can be no new variables that have to be quantified!!
          val allFV = FreeVariables.freeVariables(jl)
          Seq(ExistsJudgment(vl filter (allFV contains _), jl))
        }
      }
      case e @ ForallJudgment(vl, jl) => {
        if (jl.isEmpty)
          Seq() //remove construct entirely if body became empty
        else {
          //keep only the quantified variables that are still in the body
          //there can be no new variables that have to be quantified!!
          val allFV = FreeVariables.freeVariables(jl)
          Seq(ForallJudgment(vl filter (allFV contains _), jl))
        }
      }
      case OrJudgment(orc) => removeFromOrImpl(orc)
      case n @ NotJudgment(j) => {
        j match {
          case f @ FunctionExpJudgment(eq @ FunctionExpEq(_, _)) if (removeNamingPremise(eq)) => Seq()
          case _ => Seq(n)
        }
      }
    }

  private def removeFromOrImpl(ors: Seq[Seq[TypingRuleJudgment]]): Seq[TypingRuleJudgment] = {
    val concs = ors filterNot (s => (s.size == 1) && s.head.isInstanceOf[NotJudgment])
    if (concs.isEmpty)
      Seq(OrJudgment(ors))
    else {
      val prems = ors filter (s => (s.size == 1) && s.head.isInstanceOf[NotJudgment])
      //remove implication premises that are marked as substitution equations
      val newprems = prems filterNot {
        case Seq(NotJudgment(FunctionExpJudgment(eq @ FunctionExpEq(_, _)))) if (removeNamingPremise(eq)) => true
        case _ => false
      }

      // traverse remaining cases
      val restors = (newprems ++ concs) map (s => s flatMap transTypingRuleJudgments)
      if (restors.size == 0)
        Seq()
      else if (restors.size == 1)
        restors.head
      else Seq(OrJudgment(restors))
    }
  }

}

//does not remove equality premises used for substitution!
object InlineEverythingOnce extends InlineSubformulas

//fixpoint iteration of InlineEverythingOnce
object InlineEverythingFP extends ModuleTransformation {
  override def apply(m: Seq[Module]): Seq[Module] = {
    val newmd = InlineEverythingOnce(m)
    if (newmd != m) apply(newmd)
    else newmd
  }
}

//first apply InlineEverythingOnce
//at the end remove the premises that were used for substitution
//from all implications (aka typing rules)
object InlineEverythingAndRemovePrems extends ModuleTransformation {
  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    val newtr = InlineEverythingOnce.transTypingRules(tr)
    val RemPremInst = new RemoveUnusedPremises(InlineEverythingOnce.substitutedVars)
    newtr flatMap RemPremInst.transTypingRules
  }
}

//fixpoint iteration of InlineEverythingAndRemomePrems
object InlineEverythingAndRemovePremsFP extends ModuleTransformation {
  override def apply(m: Seq[Module]): Seq[Module] = {
    val newmd = InlineEverythingAndRemovePrems(m)
    if (newmd != m) apply(newmd)
    else newmd
  }
}

