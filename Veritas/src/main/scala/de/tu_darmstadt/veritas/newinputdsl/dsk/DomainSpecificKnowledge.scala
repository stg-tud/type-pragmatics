package de.tu_darmstadt.veritas.newinputdsl.dsk

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionEq}

trait DomainSpecificKnowledge {

  def attachedProperties: Map[(FunctionDef, String), TypingRule]
  def propertiesNeeded: Map[TypingRule, Seq[FunctionEq]]
  def recursiveFunctions: Map[FunctionDef, DataType]
  // A toplevel property is a property that is not needed by any other function
  def toplevelProperties: Map[(FunctionDef, String), TypingRule] = attachedProperties.filter { case ((fdef, propName), prop) =>
      propertiesNeeded.keys.forall( _ != prop)
  }

  def typesOfMetaVars: Map[(TypingRule, String), DataType]

  def expressions: Seq[DataType]
  def contexts: Seq[DataType]
  def types: Seq[DataType]

}

trait FailableDomainSpecificKnowledge extends DomainSpecificKnowledge {
  def failableTypes: Seq[DataType]

  def isFailableFunction(fun: FunctionDef): Boolean =
    failableTypes.exists {
      fun.signature.out.name == _.name
    }

  def isFailableCheckFunction(fun: FunctionDef): Boolean =
    failableTypes.exists { ft =>
      val returnsBool = fun.signature.out == SortRef("Bool")
      val hasOnlyFailableTypeParam = fun.signature.in.size == 1 && fun.signature.in.head.name == ft.name
      returnsBool && hasOnlyFailableTypeParam
    }

  def progressProperties: Map[FunctionDef, TypingRule]
  def preservationProperties: Map[FunctionDef, TypingRule]
}
