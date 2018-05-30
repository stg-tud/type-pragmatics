package de.tu_darmstadt.veritas.scalaspl.translatorfiles

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification

object FunctionFailTypeParams extends ScalaSPLSpecification {
  sealed trait Num extends Expression
  case class zero() extends Num
  case class succ(n: Num) extends Num

  def fun[T](x: Num): Num = x match {
    case zero() => zero()
  }

}
