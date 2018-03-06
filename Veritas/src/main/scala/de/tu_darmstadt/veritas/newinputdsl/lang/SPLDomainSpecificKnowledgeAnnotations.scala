package de.tu_darmstadt.veritas.newinputdsl.lang

import scala.annotation.Annotation

trait SPLDomainSpecificKnowledgeAnnotations {

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

object SPLDomainSpecificKnowledgeAnnotations {
  val annotationsIngoringFunction: Seq[String] =
    Seq("Property")
}
