package de.tu_darmstadt.veritas.scalaspl.typechecker

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._

trait StructureChecker {

  def check(bottom: FunctionExpMeta, top: FunctionExpMeta): Boolean = (bottom, top) match {
    case (_:FunctionExp, meta: FunctionMeta) => true
    case (FunctionMeta(_), _:FunctionExp) => false
    case (bottom: FunctionExp, top: FunctionExp) => check(bottom, top)
  }

  def check(bottom: FunctionExp, top: FunctionExp): Boolean = (bottom, top) match {
    case (FunctionExpApp(nameB, argsB), FunctionExpApp(nameT, argsT)) if nameB == nameT =>
      argsB.zip(argsT).forall { case (b, t) => check(b, t) }
    case (FunctionExpNot(b), FunctionExpNot(t)) =>
      check(b, t)
    case (FunctionExpEq(bl, br), FunctionExpEq(tl, tr)) =>
      check(bl, tl) && check(br, tr)
    case (FunctionExpNeq(bl, br), FunctionExpNeq(tl, tr)) =>
      check(bl, tl) && check(br, tr)
    case (FunctionExpAnd(bl, br), FunctionExpAnd(tl, tr)) =>
      check(bl, tl) && check(br, tr)
    case (FunctionExpOr(bl, br), FunctionExpOr(tl, tr)) =>
      check(bl, tl) && check(br, tr)
    case (FunctionExpBiImpl(bl, br), FunctionExpBiImpl(tl, tr)) =>
      check(bl, tl) && check(br, tr)
    case (FunctionExpLet(bname, bnamed, bin), FunctionExpLet(tname, tnamed, tin)) if bname == tname =>
      check(bnamed, tnamed) && check(bin, tin)
    case (FunctionExpTrue, FunctionExpTrue) => true
    case (FunctionExpFalse, FunctionExpFalse) => true
    case (FunctionExpVar(bname), FunctionExpVar(tname)) if bname == tname => true
    case (FunctionExpIf(bcond, bthn, belse), FunctionExpIf(tcond, tthn, telse)) =>
      check(bcond, tcond) && check(bthn, tthn) && check(belse, telse)
    case _ => false
  }
}

object ExpressionStructureChecker extends StructureChecker {
  def check(bottom: TypingRuleJudgment, top: TypingRuleJudgment): Boolean =
    (bottom, top) match {
      case (bottom: FunctionExpJudgment, top: FunctionExpJudgment) =>
        check(bottom.f, top.f)
      case (bottom: TypingJudgment, top: TypingJudgment) =>
        check(bottom.f2, top.f2)
      case (bottom: TypingJudgmentSimple, top: TypingJudgmentSimple) =>
        check(bottom.f1, top.f1)
      case _ => false
    }
}

