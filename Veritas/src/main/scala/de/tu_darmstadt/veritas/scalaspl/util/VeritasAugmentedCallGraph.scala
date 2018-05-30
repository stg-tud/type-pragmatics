package de.tu_darmstadt.veritas.scalaspl.util

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionEq, FunctionExp, FunctionExpMeta}

case class VeritasAugmentedCallGraph() extends AugmentedCallGraph[FunctionEq, FunctionExp, FunctionExpMeta] {
  override protected def getRHSOfEquation(eq: FunctionEq): FunctionExpMeta = eq.right
}
