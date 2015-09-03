package de.tu_darmstadt.veritas.backend.util

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._

object FreeVariables {
  def freeVariables(jdgmnts: Seq[TypingRuleJudgment], ignore: Seq[MetaVar] = Seq()): Set[MetaVar]
     = jdgmnts.foldLeft(Set.empty[MetaVar])((already, r) => already ++ freeVariables(r, ignore))
  
  def freeVariables(jdg: TypingRuleJudgment, ignore: Seq[MetaVar]): Set[MetaVar] = jdg match {
    case TypingJudgment(f1, f2, f3)   => freeVariables(f1, ignore) ++ freeVariables(f2, ignore) ++ freeVariables(f3, ignore)
    case TypingJudgmentSimple(f1, f2) => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpJudgment(f)       => freeVariables(f, ignore)
    case ExistsJudgment(alreadyBoundVars, jdgs) => freeVariables(jdgs, ignore ++ alreadyBoundVars)
    case ForallJudgment(alreadyBoundVars, jdgs) => freeVariables(jdgs, ignore ++ alreadyBoundVars)
    case ReduceJudgment(f1, f2) => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case NotJudgment(jdg) => freeVariables(jdg, ignore)
    case OrJudgment(jdgs) => ???
  }
  
  def freeVariables(f: FunctionExpMeta, ignore: Seq[MetaVar]): Set[MetaVar] = f match {
    case FunctionMeta(v) if (ignore contains v) => Set()
    case FunctionMeta(v) => Set(v)
    case FunctionExpNot(f) => freeVariables(f, ignore)
    case FunctionExpEq(f1, f2) => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpNeq(f1, f2) => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpAnd(f1, f2) => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpOr(f1, f2) => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpBiImpl(f1, f2) => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpIf(cond, f1, f2) => freeVariables(cond, ignore) ++ freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpLet(_, f1, f2) => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpApp(_, args@_*) => args.foldLeft(Set.empty[MetaVar])((already, r) => already ++ freeVariables(r, ignore))
    case FunctionExpVar(_) | FunctionExpTrue | FunctionExpFalse => Set()
  }

  def apply(jdgmnts: Seq[TypingRuleJudgment]) = freeVariables(jdgmnts)
}
