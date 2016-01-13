package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.util.FreeVariables
import de.tu_darmstadt.veritas.backend.Configuration

/*
 * trait for collecting equations/substitutions that can be inlined into other formulas
 */
trait CollectInlineEquations extends ModuleTransformation {

  //collect substitutions for inlining of meta variables
  var chosenSubstitutions: Map[MetaVar, FunctionExpMeta] = Map()
  var freshNames = new FreshNames

  /**
   * override to control which equations can be chosen for being inlined
   *
   * can use information from path variable, for example
   * parameter vc is the construct itself
   */
  def checkConstruct(vc: VeritasConstruct): Boolean = true

  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    //make sure that any mutable state is initialized upon application!
    chosenSubstitutions = Map()
    freshNames = new FreshNames
    super.apply(m)
  }

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    //the traversal of the sub-constructs (TypingRuleJudements of premises and conclusion) 
    //will collect named subformulas as premises
    //and modify variable chosenSubstitutions
    //reset chosenSubstitutions & freshNames for each new typing rule that is traversed
    chosenSubstitutions = Map()
    freshNames = new FreshNames
    tr match {
      case t @ TypingRule(n, prems, conss) => super.transTypingRules(t)
    }
  }

  //remove not useful and cyclic substitutions
  //i.e. only keep substitutions of the form metavar = non-metavar
  //  private def cleanSubstMap(): Unit = {
  //    var precleanedMap: Map[MetaVar, FunctionExpMeta] = Map()
  //    for ((k, v) <- chosenSubstitutions) {
  //      v match {
  //        case FunctionMeta(m) => {
  //          var lookedup: Set[MetaVar] = Set()
  //          var nextlookup = m
  //          var continue = true
  //          var res: FunctionExpMeta = FunctionMeta(m)
  //          while (continue && chosenSubstitutions.isDefinedAt(nextlookup) && !(lookedup contains m)) {
  //            lookedup += nextlookup
  //            chosenSubstitutions(m) match {
  //              case FunctionMeta(m1) => { nextlookup = m1; res = FunctionMeta(m1) }
  //              case fexp             => { continue = false; res = fexp }
  //            }
  //            precleanedMap = chosenSubstitutions.updated(k, res)
  //          }
  //        }
  //        case _ => ;
  //      }
  //    }
  //    chosenSubstitutions = precleanedMap filter {case (k, v) => !v.isInstanceOf[FunctionMeta]}
  //  }

  private def addEqtoSubstMap(eq: FunctionExpMeta): Unit = {
    if (checkConstruct(eq))
      eq match {
        case FunctionExpEq(FunctionMeta(m1), FunctionMeta(m2)) => {
          if (!chosenSubstitutions.isDefinedAt(m1))
            chosenSubstitutions = chosenSubstitutions + (m1 -> FunctionMeta(m2))
            //only include left-to-right-direction in this case!
//          if (!chosenSubstitutions.isDefinedAt(m2))
//            chosenSubstitutions = chosenSubstitutions + (m2 -> FunctionMeta(m1))
        }
        case FunctionExpEq(FunctionMeta(m), r) =>
          if (!chosenSubstitutions.isDefinedAt(m))
            chosenSubstitutions = chosenSubstitutions + (m -> r)
        case FunctionExpEq(l, FunctionMeta(m)) =>
          if (!chosenSubstitutions.isDefinedAt(m))
            chosenSubstitutions = chosenSubstitutions + (m -> l)
        case _ => ;
      }
  }

  // ensures correct scoping of substitutions (substitutions from inner scope are not visible in outer scope)
  private def typingRuleMatching(): PartialFunction[TypingRuleJudgment, TypingRuleJudgment] =
    {
      case f @ FunctionExpJudgment(eq) => {
        addEqtoSubstMap(eq) //will always contain the binding for the first (!) instance of a meta variable m within a premise list
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
        //TODO carefully think about substitution in ors!
        //only substitute within or cases - substitution within a case is ok!
        //do not attempt substitution between or cases for now
        //unsure how to do this such that it is always correct
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
        chosenSubstitutions = oldchosen
        res
      }
    }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    if (typingRuleMatching().isDefinedAt(trj))
      typingRuleMatching()(trj)
    else
      super.transTypingRuleJudgment(trj)

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    if (typingRuleMatching().isDefinedAt(trj))
      Seq(typingRuleMatching()(trj))
    else
      super.transTypingRuleJudgments(trj)

}

trait InlineSubformulas extends ModuleTransformation with CollectInlineEquations {
  /**
   * collect variables that were eliminated during inlining
   */
  var eliminatedVars: Set[MetaVar] = Set()

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    //reset state for each typing rule that is traversed
    eliminatedVars = Set()
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
    //prevent substitutions within the substitution equations themselves
    val relparent = if (vc.isInstanceOf[MetaVar] && path.isDefinedAt(2)) path(2) else path(1)
    relparent match {
      case FunctionExpEq(FunctionMeta(m), r) if (chosenSubstitutions.isDefinedAt(m) && chosenSubstitutions(m) == r) => false
      case FunctionExpEq(l, FunctionMeta(m)) if (chosenSubstitutions.isDefinedAt(m) && chosenSubstitutions(m) == l) => false
      case _ => true
    }
  }

  //substitute meta vars with meta vars in quantified varlists
  //add substituted vars to eliminatedVars - but make sure to remove vars from 
  //eliminatedVars if they don't get substituted
  private def inlineMeta(m: MetaVar): MetaVar = {
    def acyclicMVLookup(m: MetaVar, lookedup: Set[MetaVar]): MetaVar = {
      if (chosenSubstitutions.isDefinedAt(m) && checkSubstitute(m))
        chosenSubstitutions(m) match {
          case FunctionMeta(mv) if (!(lookedup contains mv)) =>
            { eliminatedVars += m; acyclicMVLookup(mv, (lookedup + m)) }
          case FunctionMeta(mv) => { eliminatedVars -= m; m }
          case _                => { eliminatedVars -= m; m }
        }
      else { eliminatedVars -= m; m }
    }
    acyclicMVLookup(m, Set())
  }

  override def transMetaVar(m: MetaVar): MetaVar =
    inlineMeta(m)

  override def transMetaVars(m: MetaVar): Seq[MetaVar] =
    Seq(inlineMeta(m))

  //look given meta-variable up as far as possible 
  //(without going into cycles and without returning sth. that contains any of
  //the meta-variables that were already looked up during a single look up)
  private def acyclicLookup(m: MetaVar, lookedup: Set[MetaVar]): FunctionExpMeta = {
    if (chosenSubstitutions.isDefinedAt(m))
      chosenSubstitutions(m) match {
        case FunctionMeta(mv) if (!(lookedup contains mv)) =>
          acyclicLookup(mv, (lookedup + m))
        case FunctionMeta(mv) => FunctionMeta(m)
        case fmexp if (!((lookedup + m) exists (mv => occursIn(FunctionMeta(mv), fmexp)))) => fmexp
        case _ => FunctionMeta(m) //i.e. do not attempt to substitute equations like x = f(x) into anything
      }
    else
      FunctionMeta(m)
  }

  private def occursIn(fm: FunctionMeta, res: FunctionExpMeta): Boolean =
    (FreeVariables.freeVariables(res, Set[MetaVar]())) contains fm.metavar

  private def substFunctionExpMeta: PartialFunction[FunctionExpMeta, FunctionExpMeta] = {
    case fm @ FunctionMeta(m) if (checkSubstitute(fm) && chosenSubstitutions.isDefinedAt(m)) =>
      {
        val res = acyclicLookup(m, Set())
        if (res != fm && !occursIn(fm, res))
          eliminatedVars += m
        else
          eliminatedVars -= m
        res
      }
    case fm @ FunctionMeta(m) => { eliminatedVars -= m; fm }
  }

  private def substFunctionExpMetas: PartialFunction[FunctionExpMeta, Seq[FunctionExpMeta]] =
    { case fm @ FunctionMeta(m) => Seq(substFunctionExpMeta(fm)) }

  //in FunctionExpMeta, substitute meta vars with what is given in chosenSubstitution
  //make sure that eliminatedVars contains exactly the variables that were present in f 
  //prior to the call, but are not present anymore after the call
  override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta =
    withSuper(super.transFunctionExpMeta(f))(substFunctionExpMeta)

  override def transFunctionExpMetas(f: FunctionExpMeta): Seq[FunctionExpMeta] =
    withSuper[FunctionExpMeta](super.transFunctionExpMetas(f))(substFunctionExpMetas)

}

/**
 * trait to control which equations used for substitution
 * will be removed from the a typing rule
 *
 * takes as parameter a set with variables that were substituted during a previous transformation
 */
class RemoveSubstitutionPremises(eliminatedVars: Set[MetaVar]) extends ModuleTransformation {
  /**
   * override to control which of the equations marked for inlining
   * get actually removed from the premises
   *
   * default: remove all premises that were used at least once during substitution
   * (but leave premises that were never used for substitution!)
   */
  def removeNamingPremise(feq: FunctionExpEq): Boolean =
    feq match {
      case FunctionExpEq(l, r) if (l == r) => true // always remove tautologies
      case FunctionExpEq(FunctionMeta(m), r) if (eliminatedVars contains m) => true
      case FunctionExpEq(l, FunctionMeta(m)) if (eliminatedVars contains m) => true
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
    }
}

//only collect equations
object CollectOnly extends CollectInlineEquations

//does not remove equality premises used for substitution!
object InlineEverythingOnce extends InlineSubformulas

//fixpoint iteration of InlineEverythingOnce
object InlineEverythingFP extends ModuleTransformation {
  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    val newmd = InlineEverythingOnce(m)
    if (newmd != m) apply(newmd)
    else newmd
  }
}

//first do fixpoint inlining
//at the end remove the unused premises
//TODO: fix this instance, cannot work like this! Recursion broken?
//from all implications (aka typing rules)
object InlineEverythingFPAndRemovePrems extends ModuleTransformation {
  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    val newtr = InlineEverythingOnce.transTypingRules(tr)
    val firstelim = InlineEverythingOnce.eliminatedVars
    if (newtr.head != tr) newtr map (ntr => InlineEverythingFPAndRemovePrems.transTypingRules(ntr))
    else newtr

    val RemPremInst = new RemoveSubstitutionPremises(firstelim)
    newtr flatMap RemPremInst.transTypingRules
  }
}



