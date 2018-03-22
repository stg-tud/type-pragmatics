package de.tu_darmstadt.veritas.newinputdsl.typechecker

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExp, FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation

case class MetaVarSubstitution(substituions: Map[FunctionMeta, FunctionExpMeta]) extends ModuleTransformation {
  override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta = f match {
    case meta: FunctionMeta =>
      if (substituions.contains(meta))
        substituions(meta)
      else
        meta
    case f: FunctionExp => transFunctionExp(f)
  }
}