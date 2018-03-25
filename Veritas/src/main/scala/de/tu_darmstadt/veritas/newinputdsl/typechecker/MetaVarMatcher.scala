package de.tu_darmstadt.veritas.newinputdsl.typechecker

import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, TypingJudgment, TypingJudgmentSimple, TypingRuleJudgment}
import de.tu_darmstadt.veritas.backend.ast.function._

trait MetaVarMatcher {
  def matchingMetaVars(bottom: TypingRuleJudgment, top: TypingRuleJudgment): Option[Map[FunctionMeta, FunctionExpMeta]] =
    (bottom, top) match {
      case (bottom: FunctionExpJudgment, top: FunctionExpJudgment) =>
        matchingMetaVars(bottom.f, top.f)
      case (bottom: TypingJudgment, top: TypingJudgment)  =>
        val contextMatches = matchingMetaVars(bottom.f1, top.f1)
        val expMatches = matchingMetaVars(bottom.f2, top.f2)
        val typeMatches = matchingMetaVars(bottom.f3, top.f3)
        if (contextMatches.nonEmpty && expMatches.nonEmpty && typeMatches.nonEmpty) {
          val firstMerge = mergeMaps(contextMatches.get, expMatches.get)
          if (firstMerge.nonEmpty)
            mergeMaps(firstMerge.get, typeMatches.get)
          else None
        } else None
      case (bottom: TypingJudgmentSimple, top: TypingJudgmentSimple)  =>
        val expMatches = matchingMetaVars(bottom.f1, top.f1)
        val typeMatches = matchingMetaVars(bottom.f2, top.f2)
        if (expMatches.nonEmpty && typeMatches.nonEmpty)
          mergeMaps(expMatches.get, typeMatches.get)
        else None
      // TODO how are these handled? are they even supported? because we cannot execute them
      // case (bottom: ForallJudgment, top: ForallJudgment)  =>
      // case (bottom: ExistsJudgment, top: ExistsJudgment)  =>
      case _ => None
    }

  private def matchingMetaVars(bottom: FunctionExpMeta, top: FunctionExpMeta): Option[Map[FunctionMeta, FunctionExpMeta]] = (bottom, top) match {
    case (_, meta: FunctionMeta) => Some(Map(meta -> bottom))
    case (FunctionMeta(_), _) =>
      throw new IllegalArgumentException("Bottom rule contained meta variable.")
    case (bottom: FunctionExp, top: FunctionExp) => matchingMetaVars(bottom, top)
  }

  private def matchingMetaVars(bottom: FunctionExp, top: FunctionExp): Option[Map[FunctionMeta, FunctionExpMeta]] = (bottom, top) match {
    case (FunctionExpApp(nameB, argsB), FunctionExpApp(nameT, argsT)) if nameB == nameT =>
      val zipped = argsB.zip(argsT)
      zipped.foldLeft[Option[Map[FunctionMeta, FunctionExpMeta]]](Some(Map())){ case (result, (left, right)) =>
        val matching = matchingMetaVars(left, right)
        if (matching.nonEmpty && result.nonEmpty)
          mergeMaps(matching.get, result.get)
        else None
      }
    case (FunctionExpNot(b), FunctionExpNot(t)) =>
      matchingMetaVars(b, t)
    case (FunctionExpEq(bl, br), FunctionExpEq(tl, tr)) =>
      matchingMetaVarsForBinary(bl, br, tl, tr)
    case (FunctionExpNeq(bl, br), FunctionExpNeq(tl, tr)) =>
      matchingMetaVarsForBinary(bl, br, tl, tr)
    case (FunctionExpAnd(bl, br), FunctionExpAnd(tl, tr)) =>
      matchingMetaVarsForBinary(bl, br, tl, tr)
    case (FunctionExpOr(bl, br), FunctionExpOr(tl, tr)) =>
      matchingMetaVarsForBinary(bl, br, tl, tr)
    case (FunctionExpBiImpl(bl, br), FunctionExpBiImpl(tl, tr)) =>
      matchingMetaVarsForBinary(bl, br, tl, tr)
    case (FunctionExpLet(bname, bnamed, bin), FunctionExpLet(tname, tnamed, tin)) if bname == tname =>
      matchingMetaVarsForBinary(bnamed, bin, tnamed, tin)
    case (FunctionExpTrue, FunctionExpTrue) => Some(Map())
    case (FunctionExpFalse, FunctionExpFalse) => Some(Map())
    case (FunctionExpVar(bname), FunctionExpVar(tname)) if bname == tname => Some(Map())
    // case (FunctionExpIf(bcond, bthen, belse), FunctionExpIf(tcond, tthen, telse)) =>
    // matchingMetaVarsForBinary(bthen, belse, tthen, telse)
    case _ => None
  }

  private def matchingMetaVarsForBinary(bl: FunctionExpMeta, br: FunctionExpMeta, tl: FunctionExpMeta, tr: FunctionExpMeta): Option[Map[FunctionMeta, FunctionExpMeta]] = {
    val left = matchingMetaVars(bl, tl)
    val right = matchingMetaVars(br, tr)
    if (left.nonEmpty && right.nonEmpty) {
      mergeMaps(left.get, right.get)
    } else None
  }

  // if both map the same meta var to different exp map exp are not equal
  private def mergeMaps(l: Map[FunctionMeta, FunctionExpMeta], r: Map[FunctionMeta, FunctionExpMeta]): Option[Map[FunctionMeta, FunctionExpMeta]] = {
    val mapsCompatible = l.forall { case (meta, exp) =>
      if (r.contains(meta))
        r(meta) == exp
      else true
    }
    if (mapsCompatible)
      Some(l ++ r)
    else None
  }
}
