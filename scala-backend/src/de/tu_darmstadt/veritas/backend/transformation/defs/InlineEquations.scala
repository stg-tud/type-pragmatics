package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.util.FreeVariables
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation

/**
 * substitute in a given construct the meta variables in substMap parameter
 * can be called for an entire module as well, but should be used inside specific
 * TypingRule or Seq[TypingRuleJudgment]
 */
class SubstituteMeta(substMap: Map[MetaVar, FunctionExpMeta]) extends ModuleTransformation {
  //TODO
}

/**
 * this trait includes some of the functionality already present in LogicalTermOptimization
 * but really ONLY removes tautologies with the given MetaVars
 * (which we should always do when inlining, because inlining creates tautologies)
 * TODO: remove this duplication of functionality somehow?
 */
class RemoveTautologies(mvlist: Seq[MetaVar]) extends ModuleTransformation {

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = tr match {
    case TypingRule(n, prems, conss) => {
      val newprems = trace(prems)(transTypingRuleJudgments(_))
      val newconss = trace(conss)(transTypingRuleJudgments(_))
      val filterprems = newprems filterNot (p => p == FunctionExpJudgment(FunctionExpTrue))
      val filterconss = newconss filterNot (p => p == FunctionExpJudgment(FunctionExpTrue))

      //ensure that no rule with empty conclusion is created
      if (filterconss.length < 1)
        Seq(TypingRule(n, filterprems, Seq(FunctionExpJudgment(FunctionExpTrue))))
      else
        Seq(TypingRule(n, filterprems, filterconss))
    }
  }

  /**
   * make sure to replace constructs with true bodies with true
   */
  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      ???
    }
  
  override def transFunctionExps(f: FunctionExp): Seq[FunctionExp] = 
    withSuper(super.transFunctionExps(f)) {
    case FunctionExpEq(f1, f2) if (f1 == f2) => Seq(FunctionExpTrue)
  }
  
  override def transFunctionExp(f: FunctionExp): FunctionExp = 
    withSuper(super.transFunctionExp(f)) {
    case FunctionExpEq(f1, f2) if (f1 == f2) => FunctionExpTrue
  }
  
  
  
}

/**
 * for discovering a single inlineable equation per scope,
 * inlining it wherever possible, and removing it
 */
trait InlineEquation extends ModuleTransformation {

  // saves current equation marked for inlining
  private var inlineableEquation: Option[FunctionExpEq] = None

  // saves current substitutions
  private var substMap: Map[MetaVar, FunctionExpMeta] = Map()

  /**
   * override to decide which function equations may be removed and which not
   * default: none may be removed
   *
   * careful: if this def does not return true or false, you have to come
   * up with a way to not run into endless loops when doing the inline fixpoint
   * iteration!
   */
  def remove(eq: FunctionExpEq): Boolean = false

  private var freshNames = new FreshNames

  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    inlineableEquation = None
    substMap = Map()
    freshNames = new FreshNames
    super.apply(m)
  }

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    inlineableEquation = None
    substMap = Map()
    freshNames = new FreshNames
    tr match {
      case tr @ TypingRule(n, prems, conss) => { //Seq(TypingRule(n, trace(prems)(transTypingRuleJudgments(_)), trace(conss)(transTypingRuleJudgments(_))))
        //look for inlineable equation in premises, outer scope
        val findprems = trace(prems)(transTypingRuleJudgments(_))
        inlineableEquation match {
          case Some(eq) => {
            // call transTypingRuleJudgment again to substitute in premises AND conclusion
            substMap += eqtoBinding(eq)
            Seq(TypingRule(n, trace(findprems)(transTypingRuleJudgments(_)), trace(conss)(transTypingRuleJudgments(_))))
          }
          // if no equation in premises found, look for inlineable equation in conclusion, outer scope
          case None => {
            if (conss.size == 1)
              Seq(tr) //do not attempt any inlining (and notably, removal) if the conclusion only has one element
            else {
              val findconss = trace(conss)(transTypingRuleJudgments(_))
              inlineableEquation match {
                case Some(eq) => {
                  // call transTypingRuleJudgment again, substitute in conclusion only
                  substMap += eqtoBinding(eq)
                  Seq(TypingRule(n, prems, trace(findconss)(transTypingRuleJudgments(_))))
                }
                case None => Seq(tr)
              }
            }
          }
        }
      }
    }
  }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    if (substMap.isEmpty) {
      findEqInTypingRuleJudgment(trj).getOrElse(super.transTypingRuleJudgment(trj))
    } else {
      substituteInTypingRuleJudgment(trj).getOrElse(super.transTypingRuleJudgment(trj))
    }

  //removes inlineable equation
  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    if (substMap.isEmpty) {
      val find = findEqInTypingRuleJudgment(trj)
      find match {
        case Some(newtrj) => Seq(newtrj)
        case None         => super.transTypingRuleJudgments(trj)
      }
    } else {
      //check first whether equation should be removed
      val removed = removeEquation(trj)
      if (removed.isEmpty) Seq()
      else {
        val subst = substituteInTypingRuleJudgment(removed.head)
        subst match {
          case Some(newtrj) => Seq(newtrj)
          case None         => Seq()
        }
      }
    }

  //substitute meta vars if substitution is defined
  override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta =
    withSuper(super.transFunctionExpMeta(f))(substFunctionExpMeta)

  override def transFunctionExpMetas(f: FunctionExpMeta): Seq[FunctionExpMeta] =
    withSuper[FunctionExpMeta](super.transFunctionExpMetas(f))(substFunctionExpMetas)

  override def transMetaVar(m: MetaVar): MetaVar =
    if (notInInlineable(m)) inlineMeta(m) else m

  override def transMetaVars(m: MetaVar): Seq[MetaVar] =
    if (notInInlineable(m)) Seq(inlineMeta(m)) else Seq(m)

  /**
   * controls which equations will be inlined
   * all equations detected as inlineable will also be removed,
   * provided methode remove returns true for the equation in question
   */
  private def checkEquation(eq: FunctionExpEq): Boolean =
    eq match {
      // both equations of the form x = x or x = y (with x != y) or removable
      case FunctionExpEq(FunctionMeta(m1), FunctionMeta(m2)) => true
      case FunctionExpEq(fm @ FunctionMeta(m), r) if (occursIn(fm, r)) => false
      case FunctionExpEq(FunctionMeta(m), r) => true
      case FunctionExpEq(l, fm @ FunctionMeta(m)) if (occursIn(fm, l)) => false
      case FunctionExpEq(l, FunctionMeta(m)) => true
      case _ => false
    }

  private def eqtoBinding(eq: FunctionExpEq): (MetaVar, FunctionExpMeta) =
    eq match {
      case FunctionExpEq(FunctionMeta(m1), FunctionMeta(m2)) => (m1, FunctionMeta(m2))
      case FunctionExpEq(FunctionMeta(m), r)                 => (m, r)
      case FunctionExpEq(l, FunctionMeta(m))                 => (m, l)
    }

  /**
   * checks whether an encountered Veritas construct is inside
   * the equation we are trying to inline
   *
   * Note 1: this should work correctly with scoping because we rename
   * all bound variables in quantifiers when entering the quantifier's body
   *
   * Note 2: this is only needed if the removable equation is not removed right away!
   */
  private def notInInlineable(vc: VeritasConstruct): Boolean = {
    //prevent substitutions within the substitution equations themselves
    val relparent = if (vc.isInstanceOf[MetaVar] && path.isDefinedAt(2)) path(2) else path(1)
    relparent match {
      case FunctionExpEq(FunctionMeta(m), r) if (substMap.isDefinedAt(m) && substMap(m) == r) => false
      case FunctionExpEq(l, FunctionMeta(m)) if (substMap.isDefinedAt(m) && substMap(m) == l) => false
      case _ => true
    }
  }

  // only overwrite inlineableEquation if no equation has been found yet!
  private def updateEquation(eq: FunctionExpEq): Unit =
    inlineableEquation match {
      case Some(_) => ;
      case None    => inlineableEquation = Some(eq)
    }

  private def processInnerScope(binders: Seq[MetaVar], body: Seq[TypingRuleJudgment]): (Seq[MetaVar], Seq[TypingRuleJudgment]) = {
    inlineableEquation match {
      case Some(_) => (Seq(), body)
      case None => {
        trace(body)(transTypingRuleJudgments(_))
        inlineableEquation match {
          case Some(eq) => {
            //substitute in body if an inlineable equation was found there
            substMap += eqtoBinding(eq)
            //rename binders, if any, to avoid variable capture
            binders foreach { m => substMap += (m -> FunctionMeta(MetaVar(freshNames.freshName(m.name)))) }
            val substbody = trace(body)(transTypingRuleJudgments(_))
            val substvl = trace(binders)(transMetaVars(_))
            //reset state variables before leaving scope
            inlineableEquation = None
            substMap = Map()
            (substvl, substbody)
          }
          case None => (Seq(), body)
        }
      }
    }
  }

  private def findEqInTypingRuleJudgment(trj: TypingRuleJudgment): Option[TypingRuleJudgment] =
    trj match {
      case fej @ FunctionExpJudgment(f) => {
        f match {
          case eq @ FunctionExpEq(l, r) => {
            if (checkEquation(eq))
              updateEquation(eq)
            Some(fej)
          }
          case _ => Some(fej)
        }
      }
      case e @ ExistsJudgment(vl, jl) => {
        val (newvl, newjl) = processInnerScope(vl, jl)
        Some(ExistsJudgment(newvl, newjl))
      }
      case f @ ForallJudgment(vl, jl) => {
        val (newvl, newjl) = processInnerScope(vl, jl)
        Some(ExistsJudgment(newvl, newjl))
      }
      //traversing NotJudgment is not beneficial, can only contain a single TypingRuleJudgment anyway
      //maybe add later: discover negations of inequalities as inlining equation as well?
      case o @ OrJudgment(orc) => {
        val neworc = orc map { sor => processInnerScope(Seq(), sor)._2 }
        Some(OrJudgment(neworc))
      }
      case _ => None
    }

  private def substituteInTypingRuleJudgment(trj: TypingRuleJudgment): Option[TypingRuleJudgment] =
    trj match {
      case FunctionExpJudgment(f) => Some(FunctionExpJudgment(trace(f)(transFunctionExp(_))))
      case ExistsJudgment(vl, jl) => {
        //to avoid capture avoidance, rename all quantifiers as well
        //by adding extra pairs to substitution
        (vl foreach { m => substMap += (m -> FunctionMeta(MetaVar(freshNames.freshName(m.name)))) })
        val newvl = trace(vl)(transMetaVars(_))
        val newjl = trace(jl)(transTypingRuleJudgments(_)) //can become empty!
        if (newjl.isEmpty)
          None
        else
          Some(ExistsJudgment(newvl, newjl))
      }
      case ForallJudgment(vl, jl) => {
        //to avoid capture avoidance, rename all quantifiers as well
        //by adding extra pairs to substitution
        (vl foreach { m => substMap += (m -> FunctionMeta(MetaVar(freshNames.freshName(m.name)))) })
        val newvl = trace(vl)(transMetaVars(_))
        val newjl = trace(jl)(transTypingRuleJudgments(_)) //can become empty!
        if (newjl.isEmpty)
          None
        else
          Some(ForallJudgment(newvl, newjl))
      }
      case OrJudgment(orc) => {
        val newors_unfiltered = orc map (sor => trace(sor)(transTypingRuleJudgments(_)))
        val newors = newors_unfiltered filter (c => !c.isEmpty)
        if (newors.isEmpty)
          None
        //TODO what about the case where newors.length = 1....? Do we have to treat this?
        //or does it not matter since fof/tff translation correctly treats this case?
        else Some(OrJudgment(newors))
      }
      case ReduceJudgment(f1, f2)       => Some(ReduceJudgment(trace(f1)(transFunctionExpMeta(_)), trace(f2)(transFunctionExpMeta(_))))
      case NotJudgment(jdg)             => Some(NotJudgment(trace(jdg)(transTypingRuleJudgment(_))))
      case TypingJudgment(f1, f2, f3)   => Some(TypingJudgment(trace(f1)(transFunctionExpMeta(_)), trace(f2)(transFunctionExpMeta(_)), trace(f3)(transFunctionExpMeta(_))))
      case TypingJudgmentSimple(f1, f2) => Some(TypingJudgmentSimple(trace(f1)(transFunctionExpMeta(_)), trace(f2)(transFunctionExpMeta(_))))
      case _                            => None //should not occur
    }

  private def removeEquation(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    trj match {
      case FunctionExpJudgment(eq @ FunctionExpEq(l, r)) =>
        inlineableEquation match {
          case Some(inl) if (eq == inl && remove(eq)) => Seq()
          case _                                      => Seq(trj)
        }
      case _ => Seq(trj)
    }

  //below code for actual substitution: 
  //whenever substitution for an encountered meta-var is found during traversal, substitute
  //exception: lookup produces cycles!

  //needed for substituting meta vars with meta vars in quantified varlists
  //do not add to eliminated vars, because we are only substituting in quantifier varlists 
  //when calling this function - so there is no need for removing any naming
  private def inlineMeta(m: MetaVar): MetaVar = {
    def acyclicMVLookup(m: MetaVar, lookedup: Set[MetaVar]): MetaVar = {
      if (substMap.isDefinedAt(m))
        substMap(m) match {
          case FunctionMeta(mv) if (!(lookedup contains mv)) => acyclicMVLookup(mv, (lookedup + m))
          case FunctionMeta(mv) => m //cycle detected, just return old metavar
          case _ => m //cannot be substituted with metavar, just return old metavar
        }
      else m
    }
    acyclicMVLookup(m, Set()) //no further substitution known, just return old metavar
  }

  //look given meta-variable up as far as possible 
  //(without going into cycles and without returning sth. that contains any of
  //the meta-variables that were already looked up during a single look up)
  //add eliminated meta variables (including the ones eliminated "on the way") to eliminatedVars accumulator
  private def acyclicLookup(m: MetaVar, lookedup: Set[MetaVar]): FunctionExpMeta = {
    if (substMap.isDefinedAt(m))
      substMap(m) match {
        case FunctionMeta(mv) if (!(lookedup contains mv)) => acyclicLookup(mv, (lookedup + m))
        case FunctionMeta(mv) => FunctionMeta(m) //cycle detected, just return old metavar
        case fmexp if (!((lookedup + m) exists (mv => occursIn(FunctionMeta(mv), fmexp)))) => fmexp
        case _ => FunctionMeta(m)
        //i.e. do not attempt to substitute equations which contain again variables that were already looked up
      }
    else
      FunctionMeta(m)
  }

  private def occursIn(fm: FunctionMeta, res: FunctionExpMeta): Boolean =
    (FreeVariables.freeVariables(res, Set[MetaVar]())) contains fm.metavar

  //only substitute meta variables that are not in inlineable equations  
  private def substFunctionExpMeta: PartialFunction[FunctionExpMeta, FunctionExpMeta] = {
    case fm @ FunctionMeta(m) if (notInInlineable(fm)) => acyclicLookup(m, Set())
  }

  private def substFunctionExpMetas: PartialFunction[FunctionExpMeta, Seq[FunctionExpMeta]] =
    { case fm @ FunctionMeta(m) => Seq(substFunctionExpMeta(fm)) }

}

object InlineOnce extends InlineEquation

object InlineAndRemoveOnce extends InlineEquation {
  override def remove(eq: FunctionExpEq): Boolean = true
}
