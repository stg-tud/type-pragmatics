package de.tu_darmstadt.veritas.scalaspl.typechecker

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpApp, FunctionExpMeta}

object ConstraintUtil {
  def removeCommonFunctionApplications(f1: FunctionExpMeta, f2: FunctionExpMeta): Set[(FunctionExpMeta, FunctionExpMeta)] =
    (f1, f2) match {
      case (FunctionExpApp(f1name, f1args), FunctionExpApp(f2name, f2args)) if f1name == f2name =>
        f1args.zip(f2args).flatMap { case (f1, f2) => removeCommonFunctionApplications(f1, f2) }.toSet
      case _ => Set((f1, f2))
    }
}
