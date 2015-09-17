package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas.VeritasConstruct
import de.tu_darmstadt.veritas.backend.veritas.FunctionPatVar
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpVar
import de.tu_darmstadt.veritas.backend.veritas.FunctionPatApp
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpApp
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation

/**
 * desugars FunctionPatVar and FunctionExpVar to FunctionPatApp/FunctionExpApp with zero arguments,
 * if they are declared constructors/consts
 */
object VarToApp0 extends ModuleTransformation {
  
  var consnames: Seq[String] = Seq()

//  override def transform: PartialFunction[VeritasConstruct, Seq[VeritasConstruct]]= {
//    case FunctionPatVar(n) => Seq(FunctionPatApp(n))
//    case FunctionExpVar(n) => Seq(FunctionExpApp(n))
//  }
//
//  override def precheck(vc: VeritasConstruct): Boolean = {
//    ???
//  }

}