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
      case NotJudgment(NotJudgment(j))                                        => j
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
      case NotJudgment(NotJudgment(j))                                        => Seq(j)
      case OrJudgment(orc) =>
        if (orc contains FunctionExpJudgment(FunctionExpFalse))
          Seq(FunctionExpJudgment(FunctionExpTrue))
        else Seq(OrJudgment(orc filterNot (_ == FunctionExpJudgment(FunctionExpTrue))))
    }

  val expressionOptimization: PartialFunction[FunctionExp, FunctionExp] = {
      case FunctionExpEq(f1, f2) if (f1 == f2)                 => FunctionExpTrue
      case FunctionExpNeq(f1, f2) if (f1 == f2)                => FunctionExpFalse
      case FunctionExpBiImpl(FunctionExpTrue, r)  => transFunctionExp(r)
      case FunctionExpBiImpl(l, FunctionExpTrue)  => transFunctionExp(l)
      case FunctionExpBiImpl(FunctionExpFalse, r) => transFunctionExp(FunctionExpNot(r))
      case FunctionExpBiImpl(l, FunctionExpFalse) => transFunctionExp(FunctionExpNot(l))
      case FunctionExpBiImpl(l, r) if (l == r)                 => FunctionExpTrue
      case FunctionExpNot(FunctionExpFalse)                    => FunctionExpTrue
      case FunctionExpNot(FunctionExpTrue)                     => FunctionExpFalse
      case FunctionExpNot(FunctionExpNot(e))                   => e
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
  
  override def transFunctionExps(f: FunctionExp): Seq[FunctionExp] =
    withSuper[FunctionExp](super.transFunctionExps(f)){case e => Seq(expressionOptimization(e))} 

  override def transFunctionExp(f: FunctionExp): FunctionExp =
    withSuper(super.transFunctionExp(f))(expressionOptimization)

}