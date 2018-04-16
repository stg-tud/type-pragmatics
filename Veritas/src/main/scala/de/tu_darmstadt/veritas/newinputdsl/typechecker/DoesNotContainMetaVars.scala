package de.tu_darmstadt.veritas.newinputdsl.typechecker

import de.tu_darmstadt.veritas.backend.ast.TypingRuleJudgment
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExp, FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation

class DoesNotContainMetaVars extends ModuleTransformation {
  def containsMetaVars: Boolean = _containsMetaVars
  var _containsMetaVars: Boolean = false

  override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta =
    f match {
      case meta: FunctionMeta =>
        _containsMetaVars = true
        meta
      case funExp: FunctionExp => super.transFunctionExp(funExp)
      case _ => throw new IllegalArgumentException("should never happen")
    }

  override def transFunctionExpMetas(f: FunctionExpMeta): Seq[FunctionExpMeta] =
    Seq(transFunctionExpMeta(f))
}

object DoesNotContainMetaVars {
  def check(f: FunctionExpMeta): Boolean = {
    val checker = new DoesNotContainMetaVars
    checker.transFunctionExpMeta(f)
    !checker.containsMetaVars
  }

  def check(trj: TypingRuleJudgment): Boolean = {
    val checker = new DoesNotContainMetaVars
    checker.transTypingRuleJudgment(trj)
    !checker.containsMetaVars
  }
}
