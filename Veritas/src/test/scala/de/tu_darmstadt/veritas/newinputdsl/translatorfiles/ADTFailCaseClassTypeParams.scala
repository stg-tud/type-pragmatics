package de.tu_darmstadt.veritas.newinputdsl.translatorfiles

import de.tu_darmstadt.veritas.newinputdsl.SPLSpecification

object ADTFailCaseClassTypeParams extends SPLSpecification {
  override def typable(context: Context, exp: Expression, typ: Typ) = true

  override def typable(exp: Expression, typ: Typ) = true

  trait First extends Expression
  case class sub[T](t: T) extends First
}
