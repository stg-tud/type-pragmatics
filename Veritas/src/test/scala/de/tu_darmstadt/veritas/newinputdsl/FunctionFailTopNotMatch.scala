package de.tu_darmstadt.veritas.newinputdsl

object FunctionFailTopNotMatch extends SPLSpecification {
  override def typable(context: FunctionFailTopNotMatch.Context, exp: FunctionFailTopNotMatch.Expression, typ: FunctionFailTopNotMatch.Typ) = ???

  override def typable(exp: FunctionFailTopNotMatch.Expression, typ: FunctionFailTopNotMatch.Typ) = ???

  trait Num extends Expression
  case class zero() extends Num
  case class succ(n: Num) extends Num
  def fun(x: Num): Num = {
    val y = x
    y match {
      case zero() => zero()
    }
  }

}
