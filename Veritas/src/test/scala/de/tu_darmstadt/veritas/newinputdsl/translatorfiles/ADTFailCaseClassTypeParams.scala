package de.tu_darmstadt.veritas.newinputdsl.translatorfiles

import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification

object ADTFailCaseClassTypeParams extends SPLSpecification {
  sealed trait First extends Expression
  case class sub[T](t: T) extends First
}
