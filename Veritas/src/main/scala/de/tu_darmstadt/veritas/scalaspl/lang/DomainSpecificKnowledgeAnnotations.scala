package de.tu_darmstadt.veritas.scalaspl.lang

import scala.annotation.Annotation

trait DomainSpecificKnowledgeAnnotations {
  case class ProgressProperty(functionName: String) extends Annotation
  case class PreservationProperty(functionName: String) extends Annotation

  // Marks an ADT that can represent a stuck state
  // Every function that returns a failable type can get stuck.
  case class FailableType() extends Annotation
  case class Property() extends Annotation
  // a function can have multiple properties and each property gets a name assigned and a function
  // first position function param pos, second ctor position of function param and so on
  case class Recursive(positions: Int*) extends Annotation {
    require(positions.nonEmpty)
  }
}

object DomainSpecificKnowledgeAnnotations {
  val annotationsIngoringFunction: Seq[String] =
    Seq("Property")
}
