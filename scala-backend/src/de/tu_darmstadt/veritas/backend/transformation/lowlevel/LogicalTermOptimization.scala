package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.util._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation

object LogicalTermOptimization extends ModuleTransformation {

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] =
    withSuper(super.transTypingRules(tr)) {
      case TypingRule(n, prems, conss) => {
        val optimizedprems = optimizeSeqTypingRuleJudgment(prems)
        val optimizedconss = optimizeSeqTypingRuleJudgment(conss)

        //throw rules with empty conclusions completely out (= conclusions trivially true)
        if (optimizedconss == Seq())
          Seq()
        else
          Seq(TypingRule(n, optimizedprems, optimizedconss))
      }
    }

  /**
   * optimize conjunctions made from sequences of TypingRuleJudgment
   */
  private def optimizeSeqTypingRuleJudgment(jl: Seq[TypingRuleJudgment]): Seq[TypingRuleJudgment] = {
    if (jl contains FunctionExpJudgment(FunctionExpFalse))
      Seq(FunctionExpJudgment(FunctionExpFalse))
    else
      jl filterNot (trj => (trj == FunctionExpJudgment(FunctionExpTrue)))
  }

  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment =
    withSuper(super.transTypingRuleJudgment(trj)) {
      case ExistsJudgment(vl, jl) => {
        val optimizedjl = optimizeSeqTypingRuleJudgment(jl)
        optimizedjl match {
          case Seq() => FunctionExpJudgment(FunctionExpTrue)
          case Seq(f @ FunctionExpJudgment(FunctionExpFalse)) => f
          case seq => {
            //remove superfluous binders
            val newvl = vl filter (mv => FreeVariables.freeVariables(seq, Set[MetaVar]()) contains mv)
            //this potentially generates an exists judgment with an empty variable list
            //all subsequent transformations should be able to handle this at the moment
            //TODO is there a better solution??
            ExistsJudgment(newvl, seq)
          }
        }
      }
      case ForallJudgment(vl, jl) => {
        val optimizedjl = optimizeSeqTypingRuleJudgment(jl)
        optimizedjl match {
          case Seq() => FunctionExpJudgment(FunctionExpTrue)
          case Seq(f @ FunctionExpJudgment(FunctionExpFalse)) => f
          case seq => {
            //remove superfluous binders
            val newvl = vl filter (mv => FreeVariables.freeVariables(seq, Set[MetaVar]()) contains mv)
            //this potentially generates an exists judgment with an empty variable list
            //all subsequent transformations should be able to handle this at the moment
            //TODO is there a better solution??
            ForallJudgment(newvl, seq)
          }
        }
      }
      case NotJudgment(f @ FunctionExpJudgment(FunctionExpTrue))  => FunctionExpJudgment(FunctionExpFalse)
      case NotJudgment(f @ FunctionExpJudgment(FunctionExpFalse)) => FunctionExpJudgment(FunctionExpTrue)
      case NotJudgment(NotJudgment(j))                            => j
      case OrJudgment(orc) => {
        val optimized_orcs = orc map { ors => optimizeSeqTypingRuleJudgment(ors) }
        if (optimized_orcs contains Seq(FunctionExpJudgment(FunctionExpTrue)))
          FunctionExpJudgment(FunctionExpTrue)
        else {
          val filtered_orcs = optimized_orcs filterNot (_ == Seq(FunctionExpJudgment(FunctionExpFalse)))
          if (filtered_orcs.isEmpty)
            FunctionExpJudgment(FunctionExpFalse)
          //this potentially generates Ors with just one conjunction
          //TODO is there a better solution?
          else OrJudgment(filtered_orcs)
        }
      }
    }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] =
    withSuper(super.transTypingRuleJudgments(trj)) {
      case ExistsJudgment(vl, jl) => {
        val optimizedjl = optimizeSeqTypingRuleJudgment(jl)
        optimizedjl match {
          case Seq() => Seq(FunctionExpJudgment(FunctionExpTrue))
          case Seq(f @ FunctionExpJudgment(FunctionExpFalse)) => Seq(f)
          case seq => {
            //remove superfluous binders
            val newvl = vl filter (mv => FreeVariables.freeVariables(seq, Set[MetaVar]()) contains mv)
            if (newvl.isEmpty)
              seq
            else
              Seq(ExistsJudgment(newvl, seq))
          }
        }
      }
      case ForallJudgment(vl, jl) => {
        val optimizedjl = optimizeSeqTypingRuleJudgment(jl)
        optimizedjl match {
          case Seq() => Seq(FunctionExpJudgment(FunctionExpTrue))
          case Seq(f @ FunctionExpJudgment(FunctionExpFalse)) => Seq(f)
          case seq => {
            //remove superfluous binders
            val newvl = vl filter (mv => FreeVariables.freeVariables(seq, Set[MetaVar]()) contains mv)
            if (newvl.isEmpty)
              seq
            else
              Seq(ForallJudgment(newvl, seq))
          }
        }
      }
      case NotJudgment(f @ FunctionExpJudgment(FunctionExpTrue))  => Seq(FunctionExpJudgment(FunctionExpFalse))
      case NotJudgment(f @ FunctionExpJudgment(FunctionExpFalse)) => Seq(FunctionExpJudgment(FunctionExpTrue))
      case NotJudgment(NotJudgment(j))                            => Seq(j)
      case OrJudgment(orc) => {
        val filtered_orig = orc filterNot (_ == Seq())
        val optimized_orcs = filtered_orig map { ors => optimizeSeqTypingRuleJudgment(ors) }
        if ((optimized_orcs contains Seq(FunctionExpJudgment(FunctionExpTrue))) || 
            (optimized_orcs contains Seq())) //empty Seq after optimization come from true cases
          Seq(FunctionExpJudgment(FunctionExpTrue))
        else {
          val filtered_orcs = optimized_orcs filterNot (_ == Seq(FunctionExpJudgment(FunctionExpFalse)))
          if (filtered_orcs.isEmpty)
            Seq(FunctionExpJudgment(FunctionExpFalse))
          else if (filtered_orcs.length == 1)
            filtered_orcs.head
          else Seq(OrJudgment(filtered_orcs))
        }
      }
    }

  val expressionOptimization: PartialFunction[FunctionExp, FunctionExp] = {
    case FunctionExpEq(f1, f2) if (f1 == f2)                 => FunctionExpTrue
    case FunctionExpNeq(f1, f2) if (f1 == f2)                => FunctionExpFalse
    case FunctionExpBiImpl(FunctionExpTrue, r)               => transFunctionExp(r)
    case FunctionExpBiImpl(l, FunctionExpTrue)               => transFunctionExp(l)
    case FunctionExpBiImpl(FunctionExpFalse, r)              => transFunctionExp(FunctionExpNot(r))
    case FunctionExpBiImpl(l, FunctionExpFalse)              => transFunctionExp(FunctionExpNot(l))
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
    withSuper[FunctionExp](super.transFunctionExps(f)) { case e if expressionOptimization.isDefinedAt(e) => Seq(expressionOptimization(e)) }

  override def transFunctionExp(f: FunctionExp): FunctionExp =
    withSuper(super.transFunctionExp(f))(expressionOptimization)

}