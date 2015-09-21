package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.lowlevel.CollectTypeInfo
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.util.FreshNames

/**
 * introduce variable names for subformulas for which checkParents holds
 */
trait NameSubformulas extends ModuleTransformation {
  val freshNames = new FreshNames
  
  /**
   * override to make sure that only subformulas with certain parents are replaced by equations
   * default implementation replaces everything recursively!
   */
  def checkParents(): Boolean = true 
  
  /**
   * gets direct parent of subformula for which a meta variable is to be generated
   */
  def newMetaVar(parent: VeritasConstruct): MetaVar = MetaVar(freshNames.freshName("VAR"))
  
  
}