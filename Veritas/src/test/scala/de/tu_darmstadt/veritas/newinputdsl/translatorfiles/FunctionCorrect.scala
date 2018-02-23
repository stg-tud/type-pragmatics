package de.tu_darmstadt.veritas.newinputdsl.translatorfiles

import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification

object FunctionCorrect extends SPLSpecification {
  override def typable(context: FunctionCorrect.Context, exp: FunctionCorrect.Expression, typ: FunctionCorrect.Typ) = true

  override def typable(exp: FunctionCorrect.Expression, typ: FunctionCorrect.Typ) = true

  trait Num extends Expression
  case class zero() extends Num
  case class succ(n: Num) extends Num

  def pred(n: Num): Num = n match {
    case zero() => zero()
    case succ(n) => n
  }

  def predpred(n: Num): Num = n match {
    case zero() => zero()
    case succ(zero()) => zero()
    case succ(succ(n)) => n
  }

  def plus(a: Num, b: Num): Num = (a, b) match {
      //TODO check that variables only in pats are used
    case (zero(), b) => if (b == zero() && b == b || a != zero()) zero() else succ(b)
    case (a, succ(n)) => succ(plus(a, n))
  }

  trait YN extends Expression
  case class yes() extends YN
  case class no() extends YN

  def singlelet(a: Num, b: YN): YN = (a, b) match {
    case (zero(), yes()) =>
      val x = plus(zero(), succ(zero()))
      if (!(x == zero())) yes() else no()
  }

  def multiplelets(a: Num, b: YN): YN = (a, b) match {
    case (zero(), yes()) =>
      val x = plus(zero(), succ(zero()))
      val y = plus(zero(), succ(zero()))
      val z = plus(zero(), succ(zero()))
      if ((x == zero()) <==> (y == zero())) yes() else no()
  }
}
