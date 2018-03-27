package de.tu_darmstadt.veritas.newinputdsl.typechecker

import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification

trait TypeChecker[Spec <: SPLSpecification,
    Context <: Spec#Context,
    Expression <: Spec#Expression,
    Type <: Spec#Type] {

  def typable(context: Context, exp: Expression, typ: Type): Boolean
  def typable(exp: Expression, typ: Type): Boolean
}
