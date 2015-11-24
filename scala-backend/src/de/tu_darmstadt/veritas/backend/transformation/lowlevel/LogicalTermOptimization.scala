package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation

object LogicalTermOptimization extends ModuleTransformation {

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] =
    withSuper(super.transTypingRules(tr)) {
      case TypingRule(n, prems, conss) => {
        val filteredprems = prems filterNot (_ == FunctionExpJudgment(FunctionExpTrue))
        if (filteredprems == Seq())
          Seq(TypingRule(n, Seq(FunctionExpJudgment(FunctionExpTrue)), conss))
        else
          Seq(TypingRule(n, filteredprems, conss))
      }
    }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    withSuper(super.transTypingRuleJudgment(trj)) {
      case ExistsJudgment(vl, Seq(f @ FunctionExpJudgment(FunctionExpTrue)))  => f
      case ExistsJudgment(vl, Seq(f @ FunctionExpJudgment(FunctionExpFalse))) => f
      case ForallJudgment(vl, Seq(f @ FunctionExpJudgment(FunctionExpTrue)))  => f
      case ForallJudgment(vl, Seq(f @ FunctionExpJudgment(FunctionExpFalse))) => f
      case NotJudgment(f @ FunctionExpJudgment(FunctionExpTrue))              => FunctionExpJudgment(FunctionExpFalse)
      case NotJudgment(f @ FunctionExpJudgment(FunctionExpFalse))             => FunctionExpJudgment(FunctionExpTrue)
      case OrJudgment(orc) =>
        if (orc contains FunctionExpJudgment(FunctionExpFalse))
          FunctionExpJudgment(FunctionExpTrue)
        else OrJudgment(orc filterNot (_ == FunctionExpJudgment(FunctionExpTrue)))
    }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      case ExistsJudgment(vl, Seq(f @ FunctionExpJudgment(FunctionExpTrue)))  => Seq(f)
      case ExistsJudgment(vl, Seq(f @ FunctionExpJudgment(FunctionExpFalse))) => Seq(f)
      case ForallJudgment(vl, Seq(f @ FunctionExpJudgment(FunctionExpTrue)))  => Seq(f)
      case ForallJudgment(vl, Seq(f @ FunctionExpJudgment(FunctionExpFalse))) => Seq(f)
      case NotJudgment(f @ FunctionExpJudgment(FunctionExpTrue))              => Seq(FunctionExpJudgment(FunctionExpFalse))
      case NotJudgment(f @ FunctionExpJudgment(FunctionExpFalse))             => Seq(FunctionExpJudgment(FunctionExpTrue))
      case OrJudgment(orc) =>
        if (orc contains FunctionExpJudgment(FunctionExpFalse))
          Seq(FunctionExpJudgment(FunctionExpTrue))
        else Seq(OrJudgment(orc filterNot (_ == FunctionExpJudgment(FunctionExpTrue))))
    }

  override def transFunctionExps(f: FunctionExp): Seq[FunctionExp] =
    withSuper[FunctionExp](super.transFunctionExps(f)) {
      case FunctionExpEq(f1, f2) if (f1 == f2)                 => Seq(FunctionExpTrue)
      case FunctionExpNeq(f1, f2) if (f1 == f2)                => Seq(FunctionExpFalse)
      case FunctionExpBiImpl(l, r) if (l == r)                 => Seq(FunctionExpTrue)
      case FunctionExpNot(FunctionExpFalse)                    => Seq(FunctionExpTrue)
      case FunctionExpNot(FunctionExpTrue)                     => Seq(FunctionExpFalse)
      case FunctionExpAnd(_, FunctionExpFalse)                 => Seq(FunctionExpFalse)
      case FunctionExpAnd(FunctionExpFalse, _)                 => Seq(FunctionExpFalse)
      case FunctionExpAnd(l, FunctionExpTrue)                  => Seq(transFunctionExp(l))
      case FunctionExpAnd(FunctionExpTrue, r)                  => Seq(transFunctionExp(r))
      case FunctionExpAnd(l, r) if (l == r)                    => Seq(transFunctionExp(l))
      case FunctionExpOr(_, FunctionExpTrue)                   => Seq(FunctionExpTrue)
      case FunctionExpOr(FunctionExpTrue, _)                   => Seq(FunctionExpTrue)
      case FunctionExpOr(l, FunctionExpFalse)                  => Seq(transFunctionExp(l))
      case FunctionExpOr(FunctionExpFalse, r)                  => Seq(transFunctionExp(r))
      case FunctionExpOr(l, r) if (l == r)                     => Seq(transFunctionExp(l))
      case FunctionExpIf(FunctionExpTrue, t, e)                => Seq(transFunctionExpMeta(t).asInstanceOf[FunctionExp])
      case FunctionExpIf(FunctionExpFalse, t, e)               => Seq(transFunctionExpMeta(e).asInstanceOf[FunctionExp])
      case FunctionExpIf(c, t, e) if (t == e)                  => Seq(transFunctionExpMeta(t).asInstanceOf[FunctionExp])
      case FunctionExpIf(c, FunctionExpTrue, FunctionExpFalse) => Seq(transFunctionExpMeta(c).asInstanceOf[FunctionExp])
      case FunctionExpIf(c, FunctionExpFalse, FunctionExpTrue) => Seq(FunctionExpNot(transFunctionExpMeta(c).asInstanceOf[FunctionExp]))
    }

  override def transFunctionExp(f: FunctionExp): FunctionExp =
    withSuper(super.transFunctionExp(f)) {
      case FunctionExpEq(f1, f2) if (f1 == f2)                 => FunctionExpTrue
      case FunctionExpNeq(f1, f2) if (f1 == f2)                => FunctionExpFalse
      case FunctionExpBiImpl(l, r) if (l == r)                 => FunctionExpTrue
      case FunctionExpNot(FunctionExpFalse)                    => FunctionExpTrue
      case FunctionExpNot(FunctionExpTrue)                     => FunctionExpFalse
      case FunctionExpAnd(_, FunctionExpFalse)                 => FunctionExpFalse
      case FunctionExpAnd(FunctionExpFalse, _)                 => FunctionExpFalse
      case FunctionExpAnd(l, FunctionExpTrue)                  => transFunctionExp(l)
      case FunctionExpAnd(FunctionExpTrue, r)                  => transFunctionExp(r)
      case FunctionExpAnd(l, r) if (l == r)                    => transFunctionExp(l)
      case FunctionExpOr(_, FunctionExpTrue)                   => FunctionExpTrue
      case FunctionExpOr(FunctionExpTrue, _)                   => FunctionExpTrue
      case FunctionExpOr(l, FunctionExpFalse)                  => transFunctionExp(l)
      case FunctionExpOr(FunctionExpFalse, r)                  => transFunctionExp(r)
      case FunctionExpOr(l, r) if (l == r)                     => transFunctionExp(l)
      case FunctionExpIf(FunctionExpTrue, t, e)                => transFunctionExpMeta(t).asInstanceOf[FunctionExp]
      case FunctionExpIf(FunctionExpFalse, t, e)               => transFunctionExpMeta(e).asInstanceOf[FunctionExp]
      case FunctionExpIf(c, t, e) if (t == e)                  => transFunctionExpMeta(t).asInstanceOf[FunctionExp]
      case FunctionExpIf(c, FunctionExpTrue, FunctionExpFalse) => transFunctionExpMeta(c).asInstanceOf[FunctionExp]
      case FunctionExpIf(c, FunctionExpFalse, FunctionExpTrue) => FunctionExpNot(transFunctionExpMeta(c).asInstanceOf[FunctionExp])
    }

}