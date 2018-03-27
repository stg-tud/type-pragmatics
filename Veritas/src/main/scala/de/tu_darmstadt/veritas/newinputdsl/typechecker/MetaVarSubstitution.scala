package de.tu_darmstadt.veritas.newinputdsl.typechecker

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExp, FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation

case class MetaVarSubstitution(substituions: Map[FunctionMeta, FunctionExpMeta]) extends ModuleTransformation {
  override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta =
    f match {
      case meta: FunctionMeta =>
        if (substituions.contains(meta))
          substituions(meta)
        else
          meta
      case funExp: FunctionExp => super.transFunctionExp(funExp)
      case _ => throw new IllegalArgumentException("should never happen")
    }

  override def transFunctionExpMetas(f: FunctionExpMeta): Seq[FunctionExpMeta] =
    Seq(transFunctionExpMeta(f))
}
// case class MetaVarSubstitution(substituions: Map[FunctionMeta, FunctionExpMeta]) {
//   def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment = trj match {
//       case FunctionExpJudgment(f) => FunctionExpJudgment(substitute(f))
//       case TypingJudgment(ctx, exp, typ) =>
//         TypingJudgment(substitute(ctx), substitute(exp), substitute(typ))
//       case TypingJudgmentSimple(exp, typ) =>
//         TypingJudgmentSimple(substitute(exp), substitute(typ))
//       case _ => trj
//     }
//
//   private def substitute(funExpMeta: FunctionExpMeta): FunctionExpMeta = funExpMeta match {
//     case meta: FunctionMeta =>
//       if (substituions.contains(meta))
//         substituions(meta)
//       else
//         meta
//     case funExp: FunctionExp => substitute(funExp)
//     case _ => throw new IllegalArgumentException("should never happen")
//   }
//
//   private def substitute(funExp: FunctionExp): FunctionExp = funExp match {
//     case FunctionExpApp(name, args) => FunctionExpApp(name , args.map(substitute))
//     case FunctionExpNot(f) => FunctionExpNot(substitute(f))
//     case FunctionExpEq(l, r) => FunctionExpEq(substitute(l), substitute(r))
//     case FunctionExpNeq(l, r) => FunctionExpNeq(substitute(l), substitute(r))
//     case FunctionExpAnd(l, r) => FunctionExpAnd(substitute(l), substitute(r))
//     case FunctionExpOr(l, r) => FunctionExpOr(substitute(l), substitute(r))
//     case FunctionExpBiImpl(l, r) => FunctionExpBiImpl(substitute(l), substitute(r))
//     case FunctionExpIf(cond, thn, els) => FunctionExpIf(substitute(cond), substitute(thn), substitute(els))
//     case FunctionExpLet(name, named, in) => FunctionExpLet(name, substitute(named), substitute(in))
//     case _ => funExp

//  }
// }
