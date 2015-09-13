package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas.VeritasConstruct
import de.tu_darmstadt.veritas.backend.veritas.FunctionPatVar
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpVar
import de.tu_darmstadt.veritas.backend.veritas.FunctionPatApp
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpApp
import de.tu_darmstadt.veritas.backend.transformation.VeritasConstructTransformation

/**
 * no precondition necessary
 * desugars FunctionPatVar and FunctionExpVar to FunctionPatApp/FunctionExpApp with zero arguments
 */
object VarToApp0 extends VeritasConstructTransformation {
  override def transform: PartialFunction[VeritasConstruct, Seq[VeritasConstruct]] = {
    case FunctionPatVar(n) => Seq(FunctionPatApp(n))
    case FunctionExpVar(n) => Seq(FunctionExpApp(n))
  }

}