package de.tu_darmstadt.veritas.newinputdsl

object ADTFailTypeParams extends SPLSpecification {
  override def typable(context: Context, exp: Expression, typ: Typ) = true

  override def typable(exp: Expression, typ: Typ) = true

  @Open
  trait First[T] extends Expression
}
