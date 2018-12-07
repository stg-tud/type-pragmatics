package de.tu_darmstadt.veritas.scalaspl.typechecker

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
