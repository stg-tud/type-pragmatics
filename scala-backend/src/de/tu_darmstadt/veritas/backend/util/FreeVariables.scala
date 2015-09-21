package de.tu_darmstadt.veritas.backend.util

import de.tu_darmstadt.veritas.backend.veritas._

object FreeVariables {
  /**
   * returns the union of all unbound MetaVars inside the given judgments
   * @param ignore use e.g. because already bound and should not be captured again
   */
  def freeVariables(jdgmnts: Seq[TypingRuleJudgment], ignore: Set[MetaVar] = Set()): Set[MetaVar] = jdgmnts.foldLeft(Set.empty[MetaVar])((already, r) => already ++ freeVariables(r, ignore))

  /**
   * single TypingRuleJudgment
   */
  def freeVariables(jdg: TypingRuleJudgment, ignore: Set[MetaVar]): Set[MetaVar] = jdg match {
    case TypingJudgment(f1, f2, f3)             => freeVariables(f1, ignore) ++ freeVariables(f2, ignore) ++ freeVariables(f3, ignore)
    case TypingJudgmentSimple(f1, f2)           => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpJudgment(f)                 => freeVariables(f, ignore)
    case ExistsJudgment(alreadyBoundVars, jdgs) => freeVariables(jdgs, ignore ++ alreadyBoundVars)
    case ForallJudgment(alreadyBoundVars, jdgs) => freeVariables(jdgs, ignore ++ alreadyBoundVars)
    case ReduceJudgment(f1, f2)                 => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case NotJudgment(jdg)                       => freeVariables(jdg, ignore)
    case OrJudgment(jdgs)                       => freeVariables(jdgs.flatten, ignore)
  }

  /**
   * FunctionExp/FunctionExpMeta
   */
  def freeVariables(f: FunctionExpMeta, ignore: Set[MetaVar]): Set[MetaVar] = f match {
    case FunctionMeta(v) if (ignore contains v)                 => Set()
    case FunctionMeta(v)                                        => Set(v)
    case FunctionExpNot(f)                                      => freeVariables(f, ignore)
    case FunctionExpEq(f1, f2)                                  => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpNeq(f1, f2)                                 => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpAnd(f1, f2)                                 => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpOr(f1, f2)                                  => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpBiImpl(f1, f2)                              => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpIf(cond, f1, f2)                            => freeVariables(cond, ignore) ++ freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpLet(_, f1, f2)                              => freeVariables(f1, ignore) ++ freeVariables(f2, ignore)
    case FunctionExpApp(_, args)                                => args.foldLeft(Set.empty[MetaVar])((already, r) => already ++ freeVariables(r, ignore))
    // NOTE only collecting MetaVars not normal ExpVar
    case FunctionExpVar(_) | FunctionExpTrue | FunctionExpFalse => Set()
  }

  /**
   * convenience: can use FreeVariables(...) instead of FreeVariables.freeVariables(...)
   */
  def apply(jdgmnts: Seq[TypingRuleJudgment]) = freeVariables(jdgmnts)
}
