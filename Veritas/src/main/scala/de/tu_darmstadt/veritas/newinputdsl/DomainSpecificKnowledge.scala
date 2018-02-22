package de.tu_darmstadt.veritas.newinputdsl

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionEq}
import de.tu_darmstadt.veritas.backend.ast._

trait DomainSpecificKnowledge {

  // TODO How do we represent all goals?
  def attachedProperties: Map[(FunctionDef, String), TypingRule]
  def propertiesNeeded: Map[TypingRule, Seq[FunctionEq]]
  def recursiveFunctions: Map[FunctionDef, DataType]
  def groupings: Seq[Seq[FunctionEq]]
}
