package de.tu_darmstadt.veritas.newinputdsl.translatorfiles

import de.tu_darmstadt.veritas.newinputdsl.SPLSpecification

object FunctionFailDefFunction extends SPLSpecification {
  override def typable(context: FunctionFailDefFunction.Context, exp: FunctionFailDefFunction.Expression, typ: FunctionFailDefFunction.Typ) = true

  override def typable(exp: FunctionFailDefFunction.Expression, typ: FunctionFailDefFunction.Typ) = true

  trait Num extends Expression
  case class zero() extends Num
  case class succ(n: Num) extends Num
  def fun(x: Num): Num = x match {
    case zero() =>
      def test(): Num = {
        zero()
      }
      test()
  }
}
