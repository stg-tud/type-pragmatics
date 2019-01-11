package de.tu_darmstadt.veritas.scalaspl.util

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionEq, FunctionExp, FunctionExpMeta}

case class VeritasAugmentedCallGraph(funname: String) extends AugmentedCallGraph[FunctionEq, FunctionExp, FunctionExpMeta] {

  override val toplevel_fun: String = funname

  override protected def expressionToString(expression: FunctionExpMeta): String = expression.toPrettyString()

  override protected def getRHSOfEquation(eq: FunctionEq): FunctionExpMeta = eq.right

  override protected def makeLabelFromSingleEq(eq: FunctionEq): String = {
    val patterns = for (p <- eq.patterns) yield p.toPrettyString()
    patterns.mkString(", ")
  }

  override protected def criteriaToString(c: FunctionExp): String = c.toPrettyString()
}
