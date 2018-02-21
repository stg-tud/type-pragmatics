package de.tu_darmstadt.veritas.newinputdsl.translatorfiles

import de.tu_darmstadt.veritas.newinputdsl.SPLSpecification

object ADTFailCaseClassExpressionBase extends SPLSpecification {
  override def typable(context: ADTFailCaseClassExpressionBase.Context, exp: ADTFailCaseClassExpressionBase.Expression, typ: ADTFailCaseClassExpressionBase.Typ) = true

  override def typable(exp: ADTFailCaseClassExpressionBase.Expression, typ: ADTFailCaseClassExpressionBase.Typ) = true

  case class zero() extends Expression
}
