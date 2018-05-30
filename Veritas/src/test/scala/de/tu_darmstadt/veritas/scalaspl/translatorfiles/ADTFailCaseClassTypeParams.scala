package de.tu_darmstadt.veritas.scalaspl.translatorfiles

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification

object ADTFailCaseClassTypeParams extends ScalaSPLSpecification {
  sealed trait First extends Expression
  case class sub[T](t: T) extends First
}
