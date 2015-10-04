package de.tu_darmstadt.veritas.backend.transformation.defs

import scala.collection.mutable.Map
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.util.FreshNames

/**
 * generates inversion axioms for functions and partial functions
 * assumes that function equations have already been transformed to axioms!
 * assumes that all equations for one function are contained in a single Axioms-block
 * (uses the discovered function axioms for generating the inversion lemmas!)
 */
trait FunctionInversionAxioms extends ModuleTransformation {
  val fresh = new FreshNames
  
  /**
   * override to control for which functions inversion axioms are generated!
   * default: all! (even partial functions)
   * 
   * parameter: the function/partial function block with the function
   */
  def checkFunction(vc: VeritasConstruct): Boolean = true

}