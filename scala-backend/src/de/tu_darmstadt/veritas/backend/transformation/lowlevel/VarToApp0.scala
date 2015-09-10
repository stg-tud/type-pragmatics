package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas.ModuleDef
import de.tu_darmstadt.veritas.backend.transformation.ModuleDefTransformation

/**
 * no precondition necessary
 * desugars FunctionPatVar and FunctionExpVar to FunctionPatApp/FunctionExpApp with zero arguments
 */
object VarToApp0 extends ModuleDefTransformation {
  override protected def apply: PartialFunction[ModuleDef, Seq[ModuleDef]] = {
    ???
  }

}