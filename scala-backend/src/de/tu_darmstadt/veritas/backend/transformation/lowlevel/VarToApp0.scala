package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.VeritasConstructTransformation

/**
 * desugars FunctionPatVar and FunctionExpVar to FunctionPatApp/FunctionExpApp with zero arguments,
 * if they are declared constructors/consts
 */
object VarToApp0 extends VeritasConstructTransformation {
  var consnames: Seq[String] = Seq()

  override def transform(vc: VeritasConstruct, acc: A = initialAcc): Option[Seq[VeritasConstruct]] = vc match {
    case FunctionPatVar(n) => Some(Seq(FunctionPatApp(n)))
    case FunctionExpVar(n) => Some(Seq(FunctionExpApp(n)))
    case _                 => None
  }

  override def precheck(vc: VeritasConstruct): Boolean = {
    ???
  }

}