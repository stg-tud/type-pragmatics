package de.tu_darmstadt.veritas.newinputdsl

object ADTFailCaseClassTypeParams extends SPLSpecification {
  override def typable(context: Context, exp: Expression, typ: Typ) = true

  override def typable(exp: Expression, typ: Typ) = true

  trait First extends Expression
  case class sub[T](t: T) extends First
}
