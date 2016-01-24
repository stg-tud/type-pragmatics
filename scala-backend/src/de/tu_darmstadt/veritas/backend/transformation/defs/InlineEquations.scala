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
class SubstituteMeta(sMap: Map[MetaVar, FunctionExpMeta]) extends ModuleTransformation {
  private var freshNames = new FreshNames
  private var substMap: Map[MetaVar, FunctionExpMeta] = sMap

  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    freshNames = new FreshNames
    substMap = sMap
    super.apply(m)
  }

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    freshNames = new FreshNames
    substMap = sMap
    super.transTypingRules(tr)
  }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    substituteInTypingRuleJudgment.applyOrElse(trj, super.transTypingRuleJudgment)

  //removes inlineable equation
  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    if (substituteInTypingRuleJudgment isDefinedAt trj)
      Seq(substituteInTypingRuleJudgment(trj))
    else super.transTypingRuleJudgments(trj)

  //substitute meta vars if substitution is defined
  override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta =
    withSuper(super.transFunctionExpMeta(f))(substFunctionExpMeta)

  override def transFunctionExpMetas(f: FunctionExpMeta): Seq[FunctionExpMeta] =
    withSuper[FunctionExpMeta](super.transFunctionExpMetas(f))(substFunctionExpMetas)

  override def transMetaVar(m: MetaVar): MetaVar =
    inlineMeta(m)

  override def transMetaVars(m: MetaVar): Seq[MetaVar] =
    Seq(inlineMeta(m))

  private def substituteInTypingRuleJudgment: PartialFunction[TypingRuleJudgment, TypingRuleJudgment] =
    {
      case ExistsJudgment(vl, jl) => {
        //to avoid capture avoidance, rename all quantifiers as well
        //by adding extra pairs to substitution
        val oldSubstMap = substMap
        (vl foreach { m => substMap += (m -> FunctionMeta(MetaVar(freshNames.freshName(m.name)))) })
        val newjl = trace(jl)(transTypingRuleJudgments(_))
        val newvl_unfiltered = trace(vl)(transMetaVars(_))
        //filter unnecessary quantifiers
        val newvl = newvl_unfiltered filter (mv => FreeVariables.freeVariables(newjl, Set[MetaVar]()) contains mv)
        substMap = oldSubstMap
        if (newvl.length < 1)
          //TODO: better solution for preventing empty quantifier lists?
          ExistsJudgment(vl, newjl)
        else
          ExistsJudgment(newvl, newjl)
      }
      case ForallJudgment(vl, jl) => {
        //to avoid capture avoidance, rename all quantifiers as well
        //by adding extra pairs to substitution
        val oldSubstMap = substMap
        (vl foreach { m => substMap += (m -> FunctionMeta(MetaVar(freshNames.freshName(m.name)))) })
        val newjl = trace(jl)(transTypingRuleJudgments(_))
        val newvl_unfiltered = trace(vl)(transMetaVars(_))
        //filter unnecessary quantifiers
        val newvl = newvl_unfiltered filter (mv => FreeVariables.freeVariables(newjl, Set[MetaVar]()) contains mv)
        substMap = oldSubstMap
        if (newvl.length < 1)
          //TODO: better solution for preventing empty quantifier lists?
          ForallJudgment(vl, newjl)
        else
          ForallJudgment(newvl, newjl)
      }
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
        //substitute in substitution
        case fmexp if (!((lookedup + m) exists (mv => occursIn(FunctionMeta(mv), fmexp)))) => transFunctionExpMeta(fmexp)
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
    case fm @ FunctionMeta(m) => acyclicLookup(m, Set())
  }

  private def substFunctionExpMetas: PartialFunction[FunctionExpMeta, Seq[FunctionExpMeta]] =
    { case fm @ FunctionMeta(m) => Seq(substFunctionExpMeta(fm)) }

}

/**
 * removes tautologies for the expressions given in explist with True
 *
 * use for removing tautologies that occurred during inlining
 * why only these? -> minimal interference with logical term optimization,
 * which might or might not take place afterward
 *
 */
class RemoveTautologies(explist: Seq[FunctionExpMeta]) extends ModuleTransformation {

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = tr match {
    case TypingRule(n, prems, conss) => {
      val newprems = trace(prems)(transTypingRuleJudgments(_))
      val newconss = trace(conss)(transTypingRuleJudgments(_))
      //ensure that conclusions don't become empty
      if (newconss.length < 1)
        Seq(TypingRule(n, newprems, conss))
      else
        Seq(TypingRule(n, newprems, newconss))
    }
  }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      case FunctionExpJudgment(eq @ FunctionExpEq(_, _)) =>
        if (removeEq(eq))
          Seq()
        else
          Seq(FunctionExpJudgment(trace(eq)(transFunctionExp(_))))
      case ExistsJudgment(vl, jl) => {
        val newjl = trace(jl)(transTypingRuleJudgments(_))
        val newvl_unfiltered = trace(vl)(transMetaVars(_))
        //filter unnecessary quantifiers, if any
        val newvl = newvl_unfiltered filter (mv => FreeVariables.freeVariables(newjl, Set[MetaVar]()) contains mv)
        if (newjl.length < 1)
          Seq()
        else if (newvl.length < 1)
          newjl
        else
          Seq(ExistsJudgment(newvl, newjl))
      }
      case ForallJudgment(vl, jl) => {
        val newjl = trace(jl)(transTypingRuleJudgments(_))
        val newvl_unfiltered = trace(vl)(transMetaVars(_))
        //filter unnecessary quantifiers, if any
        val newvl = newvl_unfiltered filter (mv => FreeVariables.freeVariables(newjl, Set[MetaVar]()) contains mv)
        if (newjl.length < 1)
          Seq()
        else if (newvl.length < 1)
          newjl
        else
          Seq(ForallJudgment(newvl, newjl))
      }
      case OrJudgment(orc) =>
        {
          val neworcs_unfiltered = orc map (sor => trace(sor)(transTypingRuleJudgments(_)))
          val neworcs = neworcs_unfiltered filterNot (s => s.isEmpty)
          if (neworcs.length == 0)
            Seq()
          else if (neworcs.length == 1)
            neworcs.head
          else Seq(OrJudgment(neworcs))
        }
    }

  private def removeEq(eq: FunctionExpEq): Boolean =
    eq match {
      case FunctionExpEq(f1, f2) if (f1 == f2 && (explist contains f1)) => true
      case _ => false
    }

}

/**
 * for discovering a single inlineable equation per scope,
 * inlining it wherever possible, and removing it
 */
trait InlineEquation extends ModuleTransformation {

  // saves current equation marked for inlining
  private var inlineableEquation: Option[FunctionExpEq] = None

  //for processing quantifiers;
  //restricts which kind of naming equations may be chosen for inlining
  //in forall: only inline equations for free variables!
  //in exists: only inline equations for bound variables!
  private var restrictedToVars: Set[MetaVar] = Set()

  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    inlineableEquation = None
    restrictedToVars = Set()
    super.apply(m)
  }

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    inlineableEquation = None
    restrictedToVars = Set()
    tr match {
      case tr @ TypingRule(n, prems, conss) => {
        //look for inlineable equation in premises, outer scope
        val findprems = trace(prems)(transTypingRuleJudgments(_))
        inlineableEquation match {
          case Some(eq) => {
            val newprems = substituteInBlock(prems, eq)
            val newconss = substituteInBlock(conss, eq)
            Seq(TypingRule(n, newprems, newconss))
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
                  val newconss = substituteInBlock(conss, eq)
                  Seq(TypingRule(n, prems, newconss))
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
    findEqInTypingRuleJudgment(trj).getOrElse(super.transTypingRuleJudgment(trj))

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] = {
    val find = findEqInTypingRuleJudgment(trj)
    find match {
      case Some(newtrj) => Seq(newtrj)
      case None         => super.transTypingRuleJudgments(trj)
    }
  }

  private def substituteInBlock(trj: Seq[TypingRuleJudgment], substeq: FunctionExpEq): Seq[TypingRuleJudgment] = {
    val substitutionInst = new SubstituteMeta(Map(eqtoBinding(substeq)))
    val substtrj = trj map { trj => substitutionInst.transTypingRuleJudgment(trj) }
    val removeInst = new RemoveTautologies(Seq(substeq.f1, substeq.f2))
    substtrj flatMap { trj => removeInst.transTypingRuleJudgments(trj) }
  }

  /**
   * controls which equations will be inlined
   * all equations detected as inlineable will also be removed,
   * provided methode remove returns true for the equation in question
   */
  private def checkEquation(eq: FunctionExpEq): Boolean = {
    def checkMV(m: MetaVar): Boolean =
      if (restrictedToVars.isEmpty)
        true //if no restriction given, all MetaVars can be chosen for inlineable equations!
      else
        restrictedToVars contains m

    eq match {
      case FunctionExpEq(FunctionMeta(m1), FunctionMeta(m2)) if (checkMV(m1) && checkMV(m2)) => true
      case FunctionExpEq(fm @ FunctionMeta(m), r) if (occursIn(fm, r)) => false
      case FunctionExpEq(FunctionMeta(m), r) if (checkMV(m)) => true
      case FunctionExpEq(l, fm @ FunctionMeta(m)) if (occursIn(fm, l)) => false
      case FunctionExpEq(l, FunctionMeta(m)) if (checkMV(m)) => true
      case _ => false
    }
  }

  private def occursIn(fm: FunctionMeta, res: FunctionExpMeta): Boolean =
    (FreeVariables.freeVariables(res, Set[MetaVar]())) contains fm.metavar

  private def eqtoBinding(eq: FunctionExpEq): (MetaVar, FunctionExpMeta) =
    eq match {
      // design decision: always only substitute metavar-metavar equations from right to left
      case FunctionExpEq(FunctionMeta(m1), FunctionMeta(m2)) => (m1, FunctionMeta(m2))
      case FunctionExpEq(FunctionMeta(m), r)                 => (m, r)
      case FunctionExpEq(l, FunctionMeta(m))                 => (m, l)
    }

  // only overwrite inlineableEquation if no equation has been found yet! (in current scope)
  private def updateEquation(eq: FunctionExpEq): Unit =
    inlineableEquation match {
      case Some(_) => ;
      case None    => inlineableEquation = Some(eq)
    }

  private def processInnerScope(binders: Seq[MetaVar], body: Seq[TypingRuleJudgment]): (Seq[MetaVar], Seq[TypingRuleJudgment]) = {
    //important: we only do inlining, if the body is actually contains at least two elements!
    if (body.length > 1) {
      val oldinlineable = inlineableEquation
      inlineableEquation = None
      val findbody = trace(body)(transTypingRuleJudgments(_))
      val result = inlineableEquation match {
        case Some(eq) => {
          val newbody = substituteInBlock(body, eq)
          val newbinders = binders filter (mv => FreeVariables.freeVariables(newbody, Set[MetaVar]()) contains mv)
          (newbinders, newbody)
        }
        case None => (binders, body)
      }
      //reset inlineable equation to value before scope!
      inlineableEquation = oldinlineable
      result
    } else (binders, body)
  }

  private def findEqInTypingRuleJudgment(trj: TypingRuleJudgment): Option[TypingRuleJudgment] =
    trj match {
      case fej @ FunctionExpJudgment(f) => {
        f match {
          case eq @ FunctionExpEq(l, r) => {
            //discover an inlineable equation
            if (checkEquation(eq))
              updateEquation(eq)
            Some(fej)
          }
          case _ => Some(fej)
        }
      }
      case e @ ExistsJudgment(vl, jl) => {
        val oldrestricted = restrictedToVars
        //desired variables for inlining must be bound variables
        restrictedToVars = vl.toSet
        val (newvl, newjl) = processInnerScope(vl, jl)
        restrictedToVars = oldrestricted
        Some(ExistsJudgment(newvl, newjl))
      }
      case f @ ForallJudgment(vl, jl) => {
        val oldrestricted = restrictedToVars
        //desired variables for inlining must be free variables
        restrictedToVars = FreeVariables.freeVariables(jl, vl.toSet)
        val (newvl, newjl) = processInnerScope(vl, jl)
        restrictedToVars = oldrestricted
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
}

object InlineOnce extends InlineEquation

object InlineFP extends ModuleTransformation {
  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    val newmd = InlineOnce(m)
    if (newmd != m) apply(newmd)
    else newmd
  }
}
