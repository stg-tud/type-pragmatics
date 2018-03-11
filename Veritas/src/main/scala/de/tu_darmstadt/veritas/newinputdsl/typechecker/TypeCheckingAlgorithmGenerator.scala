package de.tu_darmstadt.veritas.newinputdsl.typechecker

import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification

trait TypeCheckingAlgorithmGenerator[Spec <: SPLSpecification,
  Context <: Spec#Context,
  Expression <: Spec#Expression,
  Type <: Spec#Typ] {

  def generate(specification: Spec): TypeChecker[Spec, Context, Expression, Type]
}

trait TypeChecker[Spec <: SPLSpecification,
  Context <: Spec#Context,
  Expression <: Spec#Expression,
  Type <: Spec#Typ] {
  // TODO via reflection we should construct a algorithms which is called by these two function in order to typecheck
  def typable(context: Context, exp: Expression, typ: Type): Boolean
  def typable(exp: Expression, typ: Type): Boolean
}
