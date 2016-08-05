package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectConstructorNames

/**
 * (blindly) desugars FunctionPatVar and FunctionExpVar to FunctionPatApp/FunctionExpApp with zero arguments
 */
object VarToApp0 extends ModuleTransformation with CollectConstructorNames {
  override def transFunctionExps(f: FunctionExp): Seq[FunctionExp] =
    withSuper[FunctionExp](super.transFunctionExps(f)) {
      case v @ FunctionExpVar(n) => if ((constNames contains n) || (constructorNames contains n)) Seq(FunctionExpApp(n, Seq())) else Seq(v)
    }

  override def transFunctionExp(f: FunctionExp): FunctionExp =
    withSuper(super.transFunctionExp(f)) {
      case v @ FunctionExpVar(n) => if ((constNames contains n) || (constructorNames contains n)) FunctionExpApp(n, Seq()) else v
    }

  override def transFunctionPatterns(p: FunctionPattern): Seq[FunctionPattern] =
    withSuper[FunctionPattern](super.transFunctionPatterns(p)) {
      case p @ FunctionPatVar(n) => if ((constNames contains n) || (constructorNames contains n)) Seq(FunctionPatApp(n, Seq())) else Seq(p)
    }
}
