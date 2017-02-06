package de.tu_darmstadt.veritas.backend.transformation.defs

import scala.collection.mutable.Map
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.util.Context

/**
 * generates axioms for function equations, throws out all lets and ifs
 * takes order of function equations into account!
 * --> assumes that FunctionPatVar/FunctionExpVar was already substituted with
 * FunctionPatApp/FunctionExpApp if there was a clash with constructor names!
 * TODO Is it possible to generate a simple precondition with this requirement? Probably not...
 */
trait FunctionEqToSimpleAxioms extends ModuleTransformation {
  var fresh = new FreshNames

  override def apply(m: Seq[Module])(implicit config: Configuration): Seq[Module] = {
    //make sure that any mutable state is initialized upon application!
    fresh = new FreshNames
    super.apply(m)
  }

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    withSuper(super.transModuleDefs(mdef)) {
      case Functions(fs) =>
        //generate one block with all the function signatures
        //then a list of axioms from all the function equations
        //join axioms for one function together to one axiom block
        Seq(Functions(fs map { case FunctionDef(sig, _) => FunctionDef(sig, Seq()) })) ++
          Seq(joinAxioms((fs flatMap { case FunctionDef(sig, feqs) => makeAxiomsEqs(feqs, sig.out, Seq()) })))
      case PartialFunctions(fs) =>
        //generate one block with all the function signatures
        //then a list of axioms from all the function equations
        //join axioms for one function together to one axiom block
        Seq(PartialFunctions(fs map { case FunctionDef(sig, _) => FunctionDef(sig, Seq()) })) ++
          Seq(joinAxioms((fs flatMap { case FunctionDef(sig, feqs) => makeAxiomsEqs(feqs, sig.out, Seq()) })))
    }

  private def joinAxioms(axlist: Seq[Axioms]): Axioms = {
    Axioms(for { Axioms(trseq) <- axlist; tr <- trseq } yield tr)
  }

  private def makeAxiomsEqs(feqs: Seq[FunctionEq], restype: SortRef, prepats: Seq[Seq[FunctionPattern]]): Seq[Axioms] = {
    feqs match {
      case Seq() => Seq()
      case Seq((f @ FunctionEq(name, pats, exp)), fs @ _*) =>
        makeAxiomsEq(f, restype, prepats) +: makeAxiomsEqs(fs, restype, prepats :+ pats)
    }
  }

  private def makeAxiomsEq(f: FunctionEq, restype: SortRef, prepats: Seq[Seq[FunctionPattern]]): Axioms = {
    val premises: Seq[Seq[TypingRuleJudgment]] = collectPremises(f, prepats)
    val conclusion: Seq[TypingRuleJudgment] = makeConclusions(f, restype)
    if ((premises.length == 0 && conclusion.length != 1) ||
      (premises.length > 0 && (premises.length != conclusion.length)))
      throw TransformationError("Transformation of the following function equation yielded different numbers of premise blocks and conclusions: " + f)

    if (premises.length == 0)
      Axioms(Seq(TypingRule(fresh.freshRuleName(f.functionName), Seq(), conclusion)))
    else
      Axioms(for ((p, c) <- premises zip conclusion) yield {
        TypingRule(fresh.freshRuleName(f.functionName), p, Seq(c))
      })
  }

  protected def collectPremises(f: FunctionEq, prepats: Seq[Seq[FunctionPattern]]): Seq[Seq[TypingRuleJudgment]] =
    f match {
      case FunctionEq(name, pats, ext) => {
        val notprepats = negatePrepats(pats, prepats)
        val iflet = collectIfLetPremises(ext)
        if (iflet.isEmpty)
          Seq(notprepats)
        else
          for (sp <- collectIfLetPremises(ext)) yield {
            notprepats ++ sp
          }
      }
    }

  protected def negatePrepats(pats: Seq[FunctionPattern], prepats: Seq[Seq[FunctionPattern]]): Seq[TypingRuleJudgment] = {
    def relevantPattern(patpair: (FunctionPattern, FunctionPattern)): Boolean =
      patpair match {
        case (_, FunctionPatVar(m)) => false
        case _                      => true
      }

    val correspondingpats = prepats map (s => pats zip s)

    val cases = correspondingpats.flatMap(sp => {
      val orcases = sp map (x =>
        if (relevantPattern(x))
          Seq(negatePatternPart(x))
        else
          Seq())
      makeOrSeqs(orcases)
    })
    cases
  }

  private def makeOrSeqs(cases: Seq[Seq[TypingRuleJudgment]]): Seq[TypingRuleJudgment] = {
    val filterempty = cases filter (o => !o.isEmpty)
    if (filterempty.length <= 1)
      filterempty.flatten
    else
      Seq(OrJudgment(filterempty))
  }

  private def negatePatternPart(patpair: (FunctionPattern, FunctionPattern)): TypingRuleJudgment = {
    val p = patpair._1
    val old = patpair._2
    val pexp = varsToMetaVars(patsToVars(p))
    val freshOld = makeFreshVars(old, new FreshNames)
    val newvars = collectVars(freshOld)
    if (newvars.isEmpty)
      FunctionExpJudgment(FunctionExpNeq(pexp, varsToMetaVars(patsToVars(freshOld))))
    else
      ForallJudgment(newvars,
        Seq(FunctionExpJudgment(FunctionExpNeq(pexp, varsToMetaVars(patsToVars(freshOld))))))
  }

  private def makeFreshVars(p: FunctionPattern, fn: FreshNames): FunctionPattern = {
    p match {
      case FunctionPatVar(n)       => FunctionPatVar(fn.freshName(n))
      case FunctionPatApp(n, args) => FunctionPatApp(n, args map (p => makeFreshVars(p, fn)))
    }
  }

  private def collectVars(p: FunctionPattern): Seq[MetaVar] =
    p match {
      case FunctionPatVar(n)       => Seq(MetaVar(n))
      case FunctionPatApp(n, args) => args flatMap collectVars
    }

  private def collectIfLetPremises(ext: FunctionExp): Seq[Seq[FunctionExpJudgment]] =
    ext match {
      case FunctionExpIf(c, t, e) => {
        val cp = makeConditionPair(c)
        val subpremsleft = collectIfLetPremises(t.asInstanceOf[FunctionExp])
        val left = subpremsleft map (s => cp._1 +: s)
        val subpremsright = collectIfLetPremises(e.asInstanceOf[FunctionExp])
        val right = subpremsright map (s => cp._2 +: s)
        left ++ right
      }
      case FunctionExpLet(n, i, e) => {
        val bodyprems = collectIfLetPremises(e.asInstanceOf[FunctionExp])
        val letpremise = makeLetPremise(n, i)
        bodyprems map (s => letpremise +: s)
      }
      case e => Seq(Seq())
      //TODO this does not support if or let statements in And, Or, Eq etc. - check whether this is a good idea!
    }

  private def makeConditionPair(c: FunctionExpMeta): (FunctionExpJudgment, FunctionExpJudgment) = {
    try {
      val pos = varsToMetaVars(c).asInstanceOf[FunctionExp]
      val neg = pos match {
        case FunctionExpEq(l, r)  => FunctionExpNeq(l, r)
        case FunctionExpNeq(l, r) => FunctionExpEq(l, r)
        case FunctionExpNot(e)    => e
        case e                    => FunctionExpNot(e)
      }
      (FunctionExpJudgment(pos), FunctionExpJudgment(neg))
    } catch {
      case e: ClassCastException  => throw TransformationError("An if condition contained unsupported top-level (meta) variables: " + c)
      case TransformationError(_) => throw TransformationError("If and let statements not supported in condition of if!")
      case e: Exception           => throw e
    }
  }

  private def makeLetPremise(n: String, i: FunctionExpMeta): FunctionExpJudgment =
    try {
      FunctionExpJudgment(FunctionExpEq(FunctionMeta(MetaVar(n)), varsToMetaVars(i)))
    } catch {
      case TransformationError(_) => throw TransformationError("If and let statements not supported in let-binding head!")
      case e: Exception           => throw e
    }

  private def makeConclusions(f: FunctionEq, restype: SortRef): Seq[TypingRuleJudgment] =
    f match {
      case FunctionEq(name, pats, ext) => {
        val patExps = pats map (p => varsToMetaVars(patsToVars(p)))
        val left = FunctionExpApp(name, patExps)
        val seprights = separateRights(ext)
        val rights = seprights map varsToMetaVars

        if (restype.name == "Bool")
          try {
            //this should work because we assume that function equations for functions with 
            //return type Bool cannot have just a variable at the right-hand side....
            for (right <- rights) yield {
              FunctionExpJudgment(FunctionExpBiImpl(left, right.asInstanceOf[FunctionExp]))
            }
          } catch {
            case c: ClassCastException => throw TransformationError(s"Function ${name} has a variable on right-hand side of an equation, not supported for functions with return type Boolean!")
            case e: Exception          => throw e
          }
        else
          for (right <- rights) yield {
            FunctionExpJudgment(FunctionExpEq(left, right))
          }
      }
    }

  /**
   * separate if branches, filter out let bodies
   * does not support if-branches or lets in FunctionExpAnd, FunctionExpApp etc.
   */
  protected def separateRights(ext: FunctionExpMeta): Seq[FunctionExpMeta] =
    ext match {
      case FunctionExpIf(c, t, e)  => separateRights(t) ++ separateRights(e)
      case FunctionExpLet(n, i, e) => separateRights(e)
      case e                       => Seq(e)
    }

  private def patsToVars(f: FunctionPattern): FunctionExpMeta =
    f match {
      case FunctionPatVar(n)       => FunctionExpVar(n)
      case FunctionPatApp(n, args) => FunctionExpApp(n, args map patsToVars)
    }

  protected def varsToMetaVars(f: FunctionExpMeta): FunctionExpMeta =
    try {
      f match {
        case FunctionExpVar(n)           => FunctionMeta(MetaVar(n))
        case m @ FunctionMeta(_)         => m //if this happens, input file was not as expected, funtion equations should not contain any MetaVars
        case FunctionExpApp(n, args)     => FunctionExpApp(n, args map varsToMetaVars)
        //casting to FunctionExp below should be ok because 
        //1) l and r cannot contain MetaVars, 
        //2) Transformation assumes that FunctionExpVars have been revolved to FunctionExpApp previously
        case FunctionExpNot(e)           => FunctionExpNot(varsToMetaVars(e).asInstanceOf[FunctionExp])
        case FunctionExpAnd(l, r)        => FunctionExpAnd(varsToMetaVars(l).asInstanceOf[FunctionExp], varsToMetaVars(r).asInstanceOf[FunctionExp])
        case FunctionExpOr(l, r)         => FunctionExpOr(varsToMetaVars(l).asInstanceOf[FunctionExp], varsToMetaVars(r).asInstanceOf[FunctionExp])
        case FunctionExpBiImpl(l, r)     => FunctionExpBiImpl(varsToMetaVars(l).asInstanceOf[FunctionExp], varsToMetaVars(r).asInstanceOf[FunctionExp])
        case FunctionExpEq(l, r)         => FunctionExpEq(varsToMetaVars(l), varsToMetaVars(r))
        case FunctionExpNeq(l, r)        => FunctionExpNeq(varsToMetaVars(l), varsToMetaVars(r))
        case t @ FunctionExpTrue         => t
        case f @ FunctionExpFalse        => f
        //the last two cases should not occur, since we transform if and let expressions...
        case i @ FunctionExpIf(c, t, e)  => throw TransformationError("varsToMetaVars encountered an untransformed If-expression in: " + i)
        case l @ FunctionExpLet(n, i, e) => throw TransformationError("varsToMetaVars encountered an untransformed Let-expression in: " + l)
      }
    } catch {
      case c: ClassCastException => throw TransformationError("In the following function expression, a construct either contained illegal meta variables, or unresolved constructor variables: " + f)
      case e: Exception          => throw e
    }
}

object FunctionEqToAxiomsSimple extends FunctionEqToSimpleAxioms

// variant which does not throw lets and ifs out when translating function equations to axioms
// generates one axiom per function equation
trait FunctionEqtoAxiomsWithLetIf extends FunctionEqToSimpleAxioms {
  override def collectPremises(f: FunctionEq, prepats: Seq[Seq[FunctionPattern]]): Seq[Seq[TypingRuleJudgment]] =
    f match {
      case FunctionEq(name, pats, ext) => {
        Seq(negatePrepats(pats, prepats))
      }
    }

  override def separateRights(ext: FunctionExpMeta): Seq[FunctionExpMeta] =
    Seq(ext)
    
  private def letBoundMetaVarstoApp(letboundVars: Set[String])(f: FunctionExpMeta): FunctionExpMeta =
   try {
      f match {
        case v@FunctionExpVar(n)           => if (letboundVars contains n) v else FunctionMeta(MetaVar(n))
        case m @ FunctionMeta(_)         => m //if this happens, input file was not as expected, function equations should not contain any MetaVars
        case FunctionExpApp(n, args)     => FunctionExpApp(n, args map letBoundMetaVarstoApp(letboundVars))
        //casting to FunctionExp below should be ok because 
        //1) l and r cannot contain MetaVars, 
        //2) Transformation assumes that FunctionExpVars have been revolved to FunctionExpApp previously
        case FunctionExpNot(e)           => FunctionExpNot(letBoundMetaVarstoApp(letboundVars)(e).asInstanceOf[FunctionExp])
        case FunctionExpAnd(l, r)        => FunctionExpAnd(letBoundMetaVarstoApp(letboundVars)(l).asInstanceOf[FunctionExp], letBoundMetaVarstoApp(letboundVars)(r).asInstanceOf[FunctionExp])
        case FunctionExpOr(l, r)         => FunctionExpOr(letBoundMetaVarstoApp(letboundVars)(l).asInstanceOf[FunctionExp], letBoundMetaVarstoApp(letboundVars)(r).asInstanceOf[FunctionExp])
        case FunctionExpBiImpl(l, r)     => FunctionExpBiImpl(letBoundMetaVarstoApp(letboundVars)(l).asInstanceOf[FunctionExp], letBoundMetaVarstoApp(letboundVars)(r).asInstanceOf[FunctionExp])
        case FunctionExpEq(l, r)         => FunctionExpEq(letBoundMetaVarstoApp(letboundVars)(l), letBoundMetaVarstoApp(letboundVars)(r))
        case FunctionExpNeq(l, r)        => FunctionExpNeq(letBoundMetaVarstoApp(letboundVars)(l), letBoundMetaVarstoApp(letboundVars)(r))
        case t @ FunctionExpTrue         => t
        case f @ FunctionExpFalse        => f
        case i @ FunctionExpIf(c, t, e)  => FunctionExpIf(letBoundMetaVarstoApp(letboundVars)(c).asInstanceOf[FunctionExp], letBoundMetaVarstoApp(letboundVars)(t), letBoundMetaVarstoApp(letboundVars)(e))
        case l @ FunctionExpLet(n, i, e) => FunctionExpLet(n, letBoundMetaVarstoApp(letboundVars ++ Set(n))(i), letBoundMetaVarstoApp(letboundVars ++ Set(n))(e))
      }
    } catch {
      case c: ClassCastException => throw TransformationError("In the following function expression, a construct either contained illegal meta variables, or unresolved constructor variables: " + f)
      case e: Exception          => throw e
    }

  override def varsToMetaVars(f: FunctionExpMeta): FunctionExpMeta =
    letBoundMetaVarstoApp(Set())(f)

}

object FunctionEqToAxiomsWLetIf extends FunctionEqtoAxiomsWithLetIf

