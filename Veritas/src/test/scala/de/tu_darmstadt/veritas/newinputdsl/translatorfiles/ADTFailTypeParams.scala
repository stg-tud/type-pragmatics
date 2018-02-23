package de.tu_darmstadt.veritas.newinputdsl.translatorfiles

import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification

object ADTFailTypeParams extends SPLSpecification {
  override def typable(context: Context, exp: Expression, typ: Typ) = true

  override def typable(exp: Expression, typ: Typ) = true

  @Open
  trait First[T] extends Expression
}
