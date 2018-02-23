package de.tu_darmstadt.veritas.newinputdsl.dskbuilderfiles

import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification

object DSKProperty extends SPLSpecification {
  override def typable(context: DSKProperty.Context, exp: DSKProperty.Expression, typ: DSKProperty.Typ) = true

  override def typable(exp: DSKProperty.Expression, typ: DSKProperty.Typ) = true

  trait Num extends Expression
  case class zero() extends Num
  case class succ(n: Num) extends Num

  def pred(n: Num): Num = n match {
    case zero() => zero()
    case succ(n) => n
  }

  @PropertyAttached("trueProperty")
  def predpred(n: Num): Num = n match {
    case zero() => zero()
    case succ(zero()) => zero()
    case succ(succ(n)) => n
  }

  @PropertyAttached("trueProperty")
  @PropertyAttached("falseProperty")
  def plus(a: Num, b: Num): Num = (a, b) match {
    case (zero(), b) => if (b == zero() && b == b || a != zero()) zero() else succ(b)
    case (a, succ(n)) => succ(plus(a, n))
  }

  trait YN extends Expression
  case class yes() extends YN
  case class no() extends YN

  @Property
  def trueProperty(a: Num, b: YN): Unit = {
  } ensuring (true)

  @Property
  def falseProperty(a: Num, b: YN): Unit = {
  } ensuring (false)

  def multiplelets(a: Num, b: YN): YN = (a, b) match {
    case (zero(), yes()) =>
      val x = plus(zero(), succ(zero()))
      val y = plus(zero(), succ(zero()))
      val z = plus(zero(), succ(zero()))
      if ((x == zero()) <==> (y == zero())) yes() else no()
  }
}
