package de.tu_darmstadt.veritas.scalaspl.util

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionEq, FunctionExp, FunctionExpMeta}

case class VeritasAugmentedCallGraph() extends AugmentedCallGraph[FunctionEq, FunctionExp, FunctionExpMeta] {
  override protected def getRHSOfEquation(eq: FunctionEq): FunctionExpMeta = eq.right

  override protected def makeLabelFromSingleEq(eq: FunctionEq): String = {
    val patterns = for (p <- eq.patterns) yield p.toPrettyString()
    patterns.mkString(", ")
  }

  override protected def criteriaToString(c: FunctionExp): String = c.toPrettyString()
}
