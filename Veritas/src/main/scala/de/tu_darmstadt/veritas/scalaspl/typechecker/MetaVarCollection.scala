package de.tu_darmstadt.veritas.scalaspl.typechecker

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExp, FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation

import scala.collection.mutable

trait MetaVarCollection extends ModuleTransformation {
  def metaVars: Set[FunctionMeta] = Set() ++ _metaVars
  private[this] val _metaVars = mutable.Set[FunctionMeta]()

  override def transFunctionExpMeta(f: FunctionExpMeta): FunctionExpMeta =
    f match {
      case meta: FunctionMeta =>
        _metaVars += meta
        meta
      case funExp: FunctionExp => super.transFunctionExp(funExp)
      case _ => throw new IllegalArgumentException("should never happen")
    }

  override def transFunctionExpMetas(f: FunctionExpMeta): Seq[FunctionExpMeta] =
    Seq(transFunctionExpMeta(f))
}
