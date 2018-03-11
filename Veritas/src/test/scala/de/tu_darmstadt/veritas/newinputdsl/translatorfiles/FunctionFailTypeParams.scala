package de.tu_darmstadt.veritas.newinputdsl.translatorfiles

import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification

object FunctionFailTypeParams extends SPLSpecification {
  trait Num extends Expression
  case class zero() extends Num
  case class succ(n: Num) extends Num

  def fun[T](x: Num): Num = x match {
    case zero() => zero()
  }

}
