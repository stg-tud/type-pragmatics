package de.tu_darmstadt.veritas.scalaspl.typechecker

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.util.FreshNames

import scala.collection.mutable

trait ConstraintBuilder {
  def constraints: Set[FunctionExpMeta] = Set() ++ _constraints

  private val _constraints: mutable.Set[FunctionExpMeta] = mutable.Set()
  private val performedSubstitutions: mutable.Map[FunctionMeta, FunctionExpMeta] = mutable.Map()
  private val freshNames = new FreshNames

  def build(bottom: TypingRuleJudgment, top: TypingRule): Option[TypingRule] = {
    val uniqueNamesTypingRule = introduceFreshNames(top)
    val consequence = build(bottom, uniqueNamesTypingRule.consequences.head)
    if (consequence.nonEmpty) {
      val subster = MetaVarSubstitution(Map() ++ performedSubstitutions)
      val premises = uniqueNamesTypingRule.premises.map { subster.transTypingRuleJudgment }
      Some(TypingRule(uniqueNamesTypingRule.name, premises, consequence.toSeq))
    } else None
  }

  private def introduceFreshNames(typingRule: TypingRule): TypingRule = {
    typingRule
    // get all metavars
    // subst them
    val collection = new MetaVarCollection { }
    collection.transTypingRules(typingRule)
    val metaVars = collection.metaVars
    val metaVarSubstitutions = metaVars.map { meta =>
      meta -> FunctionMeta(MetaVar(freshNames.freshName(meta.metavar.name + "_")))
    }.toMap
    val subster = MetaVarSubstitution(metaVarSubstitutions)
    val result = subster.transTypingRules(typingRule).head
    val funExpsWithMetaVars = result.premises.collect {
      case FunctionExpJudgment(fexp) if !DoesNotContainMetaVars.check(fexp) => fexp
    }
    _constraints ++= funExpsWithMetaVars
    result
  }

  def build(bottom: TypingRuleJudgment, top: TypingRuleJudgment): Option[TypingRuleJudgment] = (bottom, top) match {
    case (bottom: FunctionExpJudgment, top: FunctionExpJudgment) =>
      build(bottom.f, top.f).flatMap { f => Some(FunctionExpJudgment(f)) }
    case (bottom: TypingJudgment, top: TypingJudgment) =>
      val context = build(bottom.f1, top.f1)
      val expr = build(bottom.f2, top.f2)
      val typ = build(bottom.f3, top.f3)
      if (context.isEmpty || expr.isEmpty || typ.isEmpty) None
      else Some(TypingJudgment(context.get, expr.get, typ.get))
    case (bottom: TypingJudgmentSimple, top: TypingJudgmentSimple) =>
      val expr = build(bottom.f1, top.f1)
      val typ = build(bottom.f2, top.f2)
      if (expr.isEmpty || typ.isEmpty) None
      else Some(TypingJudgmentSimple(expr.get, typ.get))
    case _ => None
  }

  def build(bottom: FunctionExpMeta, top: FunctionExpMeta): Option[FunctionExpMeta] = (bottom, top) match {
    case (meta1: FunctionMeta, meta2: FunctionMeta) => Some(meta1)
    // just return meta
    case (meta: FunctionMeta, f: FunctionExp) =>
      // add constraint
      _constraints += FunctionExpEq(meta, f)
      Some(meta)
    case (f: FunctionExp, meta: FunctionMeta) =>
      // need to check if substituion is valid
      // then substitute
      if (isSubstitutionValid(meta, f)) {
        performedSubstitutions(meta) = f
        Some(f)
      } else None
    case (f1: FunctionExp, f2: FunctionExp) =>
      // if top contains metavars we need to introduce a constraint
      if (!DoesNotContainMetaVars.check(f2)) {
        val newConstraints = ConstraintUtil.removeCommonFunctionApplications(f1, f2).map { case (l, r) => FunctionExpEq(l, r) }
        _constraints ++= newConstraints
        build(f1, f2)
        Some(f2)
      } else build(f1, f2)
  }

  private def isSubstitutionValid(key: FunctionMeta, value: FunctionExpMeta): Boolean =
    if (performedSubstitutions.contains(key))
      performedSubstitutions(key) == value
    else true

  def build(bottom: FunctionExp, top: FunctionExp): Option[FunctionExp] = (bottom, top) match {
    case (FunctionExpApp(bname, bargs), FunctionExpApp(tname, targs)) if bname == tname =>
      val args = bargs.zip(targs).map { case (b, t) => build(b, t) }
      val allArgsSome = args.forall(_.nonEmpty)
      if (allArgsSome) Some(FunctionExpApp(bname, args.flatten))
      else None
    case (FunctionExpNot(b), FunctionExpNot(t)) =>
      build(b, t).flatMap { funExp => Some(FunctionExpNot(funExp)) }
    case (FunctionExpEq(bl, br), FunctionExpEq(tl, tr)) =>
      val left = build(bl, tl)
      val right = build(br, tr)
      if (left.nonEmpty && right.nonEmpty) Some(FunctionExpEq(left.get, right.get))
      else None
    case (FunctionExpNeq(bl, br), FunctionExpNeq(tl, tr)) =>
      val left = build(bl, tl)
      val right = build(br, tr)
      if (left.nonEmpty && right.nonEmpty) Some(FunctionExpNeq(left.get, right.get))
      else None
    case (FunctionExpAnd(bl, br), FunctionExpAnd(tl, tr)) =>
      val left = build(bl, tl)
      val right = build(br, tr)
      if (left.nonEmpty && right.nonEmpty) Some(FunctionExpAnd(left.get, right.get))
      else None
    case (FunctionExpOr(bl, br), FunctionExpOr(tl, tr)) =>
      val left = build(bl, tl)
      val right = build(br, tr)
      if (left.nonEmpty && right.nonEmpty) Some(FunctionExpOr(left.get, right.get))
      else None
    case (FunctionExpBiImpl(bl, br), FunctionExpBiImpl(tl, tr)) =>
      val left = build(bl, tl)
      val right = build(br, tr)
      if (left.nonEmpty && right.nonEmpty) Some(FunctionExpBiImpl(left.get, right.get))
      else None
    case (FunctionExpLet(bname, bnamed, bin), FunctionExpLet(tname, tnamed, tin)) if bname == tname =>
      val namedExpr = build(bnamed, tnamed)
      val in = build(bin, tin)
      if (namedExpr.nonEmpty && in.nonEmpty) Some(FunctionExpLet(bname, namedExpr.get, in.get))
      else None
    case (FunctionExpTrue, FunctionExpTrue) => Some(FunctionExpTrue)
    case (FunctionExpFalse, FunctionExpFalse) => Some(FunctionExpFalse)
    case (FunctionExpVar(bname), FunctionExpVar(tname)) if bname == tname => Some(bottom)
    case (FunctionExpIf(bcond, bthen, belse), FunctionExpIf(tcond, tthen, telse)) =>
      val cond = build(bcond, tcond)
      val thn = build(bthen, tthen)
      val els = build(belse, telse)
      if (cond.nonEmpty && thn.nonEmpty && els.nonEmpty)
        Some(FunctionExpIf(cond.get, thn.get, els.get))
      else None
    case _ => None
  }
}
