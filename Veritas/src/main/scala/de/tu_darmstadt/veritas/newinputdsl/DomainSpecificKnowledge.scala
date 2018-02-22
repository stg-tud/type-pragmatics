package de.tu_darmstadt.veritas.newinputdsl

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionEq}
import de.tu_darmstadt.veritas.backend.ast._

trait DomainSpecificKnowledge {

  def attachedProperties: Map[(FunctionDef, String), TypingRule]
  def propertiesNeeded: Map[TypingRule, Seq[FunctionEq]]
  def recursiveFunctions: Map[FunctionDef, DataType]
  def groupings: Seq[Seq[FunctionEq]]
  // A toplevel property is a property that is not needed by any other function
  def toplevelProperties: Map[(FunctionDef, String), TypingRule] = attachedProperties.filter { case ((fdef, propName), prop) =>
      propertiesNeeded.keys.forall( _ != prop)
  }

}
