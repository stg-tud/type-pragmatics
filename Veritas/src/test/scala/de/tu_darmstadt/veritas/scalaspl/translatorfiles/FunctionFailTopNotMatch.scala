package de.tu_darmstadt.veritas.scalaspl.translatorfiles

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification

object FunctionFailTopNotMatch extends ScalaSPLSpecification {
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
