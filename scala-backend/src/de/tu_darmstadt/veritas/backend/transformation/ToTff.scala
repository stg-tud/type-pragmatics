package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.fof._
import de.tu_darmstadt.veritas.backend.tff._
import de.tu_darmstadt.veritas.backend.veritas._


/**
 * Transforms Core TSSL (Veritas) Modules to TFF syntax
 * 
 * Structure of Core Modules
 * - no imports
 * - section with "symbol declarations" (constructor decls, const decls, function sigs...) (can be empty)
 * - section with n axioms (can be empty)
 * - exactly one goal!
 */
object ToTff {
  def toTffFile(veritasModule: Module): TffFile = {
    ???
  }
}