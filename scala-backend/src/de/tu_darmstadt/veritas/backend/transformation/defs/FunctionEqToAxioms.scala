package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.veritas.VeritasConstruct
import de.tu_darmstadt.veritas.backend.veritas.Functions
import de.tu_darmstadt.veritas.backend.veritas.FunctionDef
import de.tu_darmstadt.veritas.backend.veritas.Axioms
import de.tu_darmstadt.veritas.backend.veritas.FunctionEq
import de.tu_darmstadt.veritas.backend.transformation.VeritasConstructTransformation

/**
 * generates axioms for function equations
 * takes order of function equations into account!
 * --> assumes that FunctionPatVar/FunctionExpVar was already substituted with 
 * FunctionPatApp/FunctionExpApp if there was a clash with constructor names!
 * TODO Is it possible to generate a simple precondition with this requirement? Probably not...
 */
object FunctionEqToAxioms extends VeritasConstructTransformation {
  override def transform: PartialFunction[VeritasConstruct, Seq[VeritasConstruct]] = {
    //generate one block with all the function signatures
    //then a list of axioms from all the function equations
    case Functions(fdefs) =>
      Seq(Functions(fdefs map { case FunctionDef(sig, _) => FunctionDef(sig, Seq()) })) ++
      fdefs flatMap { case FunctionDef(_, feqs) => generateEqAxioms(feqs) }
  }

  private def generateEqAxioms(feqs: Seq[FunctionEq]): Seq[Axioms] = {
    ???
  }
}