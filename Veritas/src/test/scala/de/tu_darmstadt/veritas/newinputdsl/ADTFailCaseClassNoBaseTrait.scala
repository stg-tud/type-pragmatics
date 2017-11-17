package de.tu_darmstadt.veritas.newinputdsl

object ADTFailCaseClassNoBaseTrait extends SPLSpecification {
  override def typable(context: ADTFailCaseClassNoBaseTrait.Context, exp: ADTFailCaseClassNoBaseTrait.Expression, typ: ADTFailCaseClassNoBaseTrait.Typ) = true

  override def typable(exp: ADTFailCaseClassNoBaseTrait.Expression, typ: ADTFailCaseClassNoBaseTrait.Typ) = true

  case class zero()
}
