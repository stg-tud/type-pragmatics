package de.tu_darmstadt.veritas.newinputdsl

object FunctionFailTypeParams extends SPLSpecification {
  override def typable(context: FunctionFailTypeParams.Context, exp: FunctionFailTypeParams.Expression, typ: FunctionFailTypeParams.Typ) = ???
  override def typable(exp: FunctionFailTypeParams.Expression, typ: FunctionFailTypeParams.Typ) = ???

  trait Num extends Expression
  case class zero() extends Num
  case class succ(n: Num) extends Num

  def fun[T](x: Num): Num = x match {
    case zero() => zero()
  }

}
