package de.tu_darmstadt.veritas.backend.transformation.defs

import scala.collection.mutable.Map
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.util.FreshNames

/**
 * generates axioms for function equations
 * takes order of function equations into account!
 * --> assumes that FunctionPatVar/FunctionExpVar was already substituted with
 * FunctionPatApp/FunctionExpApp if there was a clash with constructor names!
 * TODO Is it possible to generate a simple precondition with this requirement? Probably not...
 */
trait FunctionEqToSimpleAxioms extends ModuleTransformation {
  val fresh = new FreshNames

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
      Axioms(Seq(TypingRule(fresh.freshName(f.functionName), Seq(), conclusion)))
    else
      Axioms(for ((p, c) <- premises zip conclusion) yield {
        TypingRule(fresh.freshName(f.functionName), p, Seq(c))
      })
  }

  private def collectPremises(f: FunctionEq, prepats: Seq[Seq[FunctionPattern]]): Seq[Seq[TypingRuleJudgment]] =
    f match {
      case FunctionEq(name, pats, ext) => {
        val notprepats = negatePrepats(pats, prepats)
        //val notprepats = Seq()
        for (sp <- collectIfLetPremises(ext)) yield {
          sp ++ notprepats
        }
      }
    }

  private def negatePrepats(pats: Seq[FunctionPattern], prepats: Seq[Seq[FunctionPattern]]): Seq[TypingRuleJudgment] = {
    def relevantPattern(patpair: (FunctionPattern, FunctionPattern)): Boolean =
      patpair match {
        case (FunctionPatVar(n), FunctionPatApp(m, args2)) => true
        case (FunctionPatApp(n, args1), FunctionPatApp(m, args2)) => n == m && args1.length == args2.length
        case _ => false
      }

    val correspondingpats = prepats map (s => pats zip s)
    (for {
      sp <- correspondingpats
      pp <- sp
      if (relevantPattern(pp))
    } yield negatePatternPart(pp)).flatten
  }

  private def negatePatternPart(patpair: (FunctionPattern, FunctionPattern)): Seq[TypingRuleJudgment] =
    patpair match {
      case (FunctionPatVar(n), a @ FunctionPatApp(m, args2)) => {
        val fresha = makeFreshVars(a, new FreshNames)
        Seq(ForallJudgment(collectVars(fresha),
          Seq(FunctionExpJudgment(FunctionExpNeq(FunctionMeta(MetaVar(n)), varsToMetaVars(patsToVars(fresha)))))))
      }
      case (FunctionPatApp(n, args1), FunctionPatApp(m, args2)) =>
        if (n == m)
          negatePrepats(args1, Seq(args2))
        else throw TransformationError("Could not find negation of pattern: " + patpair)
      case _ => throw TransformationError("Could not find negation of pattern: " + patpair)
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
        val left =
          if (subpremsleft.length > 0)
            (subpremsleft map (s => cp._1 +: s))
          else Seq(Seq(cp._1))
        val subpremsright = collectIfLetPremises(e.asInstanceOf[FunctionExp])
        val right =
          if (subpremsright.length > 0)
            (subpremsright map (s => cp._2 +: s))
          else Seq(Seq(cp._2))
        left ++ right
      }
      case FunctionExpLet(n, i, e) => {
        val bodyprems = collectIfLetPremises(e.asInstanceOf[FunctionExp])
        val letpremise = makeLetPremise(n, i)
        bodyprems map (s => letpremise +: s)
      }
      case e => Seq()
      //TODO this does not support if or let statements in And, Or, Eq etc. - check whether this is a good idea!
    }

  private def makeConditionPair(c: FunctionExpMeta): (FunctionExpJudgment, FunctionExpJudgment) = {
    try {
      val pos = varsToMetaVars(c).asInstanceOf[FunctionExp]
      val neg = pos match {
        case FunctionExpEq(l, r) => FunctionExpNeq(l, r)
        case e                   => FunctionExpNot(e)
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
        val left = FunctionExpApp(name, (pats map patsToVars) map varsToMetaVars)
        val seprights = separateRights(ext)
        val rights = seprights map varsToMetaVars

        if (restype.name == "Bool")
          try {
            //this should work because we assume that function equations for functions with 
            //return type Bool cannot have just a variable at the right-hand side....
            for (right <- rights) yield {
              FunctionExpJudgment(optimizeBiImpl(FunctionExpBiImpl(left, right.asInstanceOf[FunctionExp])))
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

  private def optimizeBiImpl(bimpl: FunctionExpBiImpl): FunctionExp =
    bimpl match {
      case FunctionExpBiImpl(FunctionExpTrue, r)  => r
      case FunctionExpBiImpl(l, FunctionExpTrue)  => l
      case FunctionExpBiImpl(FunctionExpFalse, r) => FunctionExpNot(r)
      case FunctionExpBiImpl(l, FunctionExpFalse) => FunctionExpNot(l)
      case b                                      => b
    }

  /**
   * separate if branches, filter out let bodies
   * does not support if-branches or lets in FunctionExpAnd, FunctionExpApp etc.
   */
  private def separateRights(ext: FunctionExpMeta): Seq[FunctionExpMeta] =
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

  private def varsToMetaVars(f: FunctionExpMeta): FunctionExpMeta =
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