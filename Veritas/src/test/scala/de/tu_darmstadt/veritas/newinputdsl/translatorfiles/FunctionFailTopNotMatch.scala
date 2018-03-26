package de.tu_darmstadt.veritas.newinputdsl.translatorfiles

import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification

object FunctionFailTopNotMatch extends SPLSpecification {
  sealed trait Num extends Expression
  case class zero() extends Num
  case class succ(n: Num) extends Num
  def fun(x: Num): Num = {
    val y = x
    y match {
      case zero() => zero()
    }
  }

}
