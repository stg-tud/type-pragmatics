package de.tu_darmstadt.veritas.scalaspl.typechecker

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification

trait
TypeCheckerGenerator[Spec <: ScalaSPLSpecification,
    Context <: Spec#Context,
    Expression <: Spec#Expression,
    Typ <: Spec#Type] {

  def generate(sourceString: String): TypeChecker[Spec, Context, Expression, Typ]
}
