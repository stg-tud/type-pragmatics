package de.tu_darmstadt.veritas.newinputdsl.translatorfiles

import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification

object FunctionFailDefFunction extends SPLSpecification {
  sealed trait Num extends Expression
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
