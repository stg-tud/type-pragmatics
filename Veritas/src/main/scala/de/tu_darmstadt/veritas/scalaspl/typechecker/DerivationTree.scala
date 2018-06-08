package de.tu_darmstadt.veritas.scalaspl.typechecker

import de.tu_darmstadt.veritas.backend.ast.{FunctionExpJudgment, TypingRuleJudgment}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpMeta, FunctionMeta}

trait DerivationTree {
  def substitue(subs: Map[FunctionMeta, FunctionExpMeta]): DerivationTree
  def check(specPath: String): Boolean
}

case class TypingRuleJudgmentNode(trj: TypingRuleJudgment, children: Set[DerivationTree]) extends DerivationTree {
  // non leaves should not contain functionexpjudgments
  override def substitue(subs: Map[FunctionMeta, FunctionExpMeta]): DerivationTree = {
    val metaVarSubstituter = MetaVarSubstitution(subs)
    val substitutedChildren = children.map { _.substitue(subs) }
    TypingRuleJudgmentNode(metaVarSubstituter.transTypingRuleJudgment(trj), substitutedChildren)
  }

  override def check(specPath: String): Boolean =
    children.forall { _.check(specPath) }
}

case class FunctionJudgmentNode(funJudgment: FunctionExpJudgment) extends DerivationTree {
  override def substitue(subs: Map[FunctionMeta, FunctionExpMeta]): DerivationTree = {
    val metaVarSubstituter = MetaVarSubstitution(subs)
    FunctionJudgmentNode(FunctionExpJudgment(metaVarSubstituter.transFunctionExp(funJudgment.f)))
  }

  override def check(specPath: String): Boolean =
    ReflectionHelper.executeFunctionExp(funJudgment.f)(specPath, Map()).asInstanceOf[Boolean]
}
