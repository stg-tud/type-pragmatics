package de.tu_darmstadt.veritas.newinputdsl.typechecker

import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification

trait TypeCheckerGenerator[Spec <: SPLSpecification,
    Context <: Spec#Context,
    Expression <: Spec#Expression,
    Typ <: Spec#Type] {

  def generate(sourceString: String): TypeChecker[Spec, Context, Expression, Typ]
}
