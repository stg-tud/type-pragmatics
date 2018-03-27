package de.tu_darmstadt.veritas.newinputdsl.lang

import scala.annotation.Annotation

trait DomainSpecificKnowledgeAnnotations {

  case class Property() extends Annotation
  // a function can have multiple properties and each property gets a name assigned and a function
  case class PropertyAttached(functionName: String) extends Annotation
  // trade-off to refer by position number because we cannot annotate a case
  case class PropertyNeeded(propertyName: String, functionEquationPositions: Int*) extends Annotation {
    require(functionEquationPositions.nonEmpty)
  }

  // TODO what happens if reduce semantics takes two arguments that need to be reduced?
  // => One solution would be that designers would be need a tuple adt
  // first position function param pos, second ctor position of function param and so on
  case class Recursive(positions: Int*) extends Annotation {
    require(positions.nonEmpty)
  }
}

object DomainSpecificKnowledgeAnnotations {
  val annotationsIngoringFunction: Seq[String] =
    Seq("Property")
}

trait FailableAnnotations extends DomainSpecificKnowledgeAnnotations {
  case class ProgressProperty(functionName: String) extends Annotation
  case class PreservationProperty(functionName: String) extends Annotation
  // Marks an ADT that can represent a stuck state
  // Every function that returns a failable type can get stuck.
  // Also every partial function can get stuck
  // Need progress when we check if failable function has not failed
  // such a function take one argument of failable and returns a boolean
  // Need preservation when pass the result of a failable function
  // to a function that can also fail and we check if it has not failed
  // TODO how do we synthesize progress and preservation properties?
  // need to know what it means to be not a value
  // need to know what it means to be welltyped
  case class FailableType() extends Annotation
}
