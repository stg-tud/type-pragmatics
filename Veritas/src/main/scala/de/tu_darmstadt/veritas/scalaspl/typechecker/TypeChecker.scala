package de.tu_darmstadt.veritas.scalaspl.typechecker

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification

trait
TypeChecker[Spec <: ScalaSPLSpecification,
    Context <: Spec#Context,
    Expression <: Spec#Expression,
    Type <: Spec#Type] {

  def typable(context: Context, exp: Expression, typ: Type): Boolean
  def typable(exp: Expression, typ: Type): Boolean
}
