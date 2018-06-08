package de.tu_darmstadt.veritas.scalaspl.dsk

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionEq}

trait DomainSpecificKnowledge {
  def failableTypes: Seq[DataType]
  def recursiveFunctions: Map[FunctionDef, DataType]
  def progressProperties: Map[FunctionDef, TypingRule]
  def preservationProperties: Map[FunctionDef, TypingRule]
}
