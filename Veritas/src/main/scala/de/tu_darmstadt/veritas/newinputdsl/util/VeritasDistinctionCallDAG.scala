package de.tu_darmstadt.veritas.newinputdsl.util

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionEq, FunctionExp, FunctionExpMeta}

case class VeritasDistinctionCallDAG() extends DistinctionCallDAG[FunctionEq, FunctionExp, FunctionExpMeta] {
  override protected def getRHSOfEquation(eq: FunctionEq): FunctionExpMeta = eq.right
}
