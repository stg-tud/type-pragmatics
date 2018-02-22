package de.tu_darmstadt.veritas.newinputdsl.dskbuilderfiles

import de.tu_darmstadt.veritas.newinputdsl.SPLSpecification

object DSKRecursive extends SPLSpecification {
  override def typable(context: Context, exp: Expression, typ: Typ) = true
  override def typable(exp: Expression, typ: Typ) = true

  trait outer2 extends Expression
  case class outerouter(outer: enclosing) extends outer2

  trait enclosing extends Expression
  case class outer(inner: inner) extends enclosing
  case class otherouter(inner: inner) extends enclosing

  trait value extends Expression
  case class valuebind() extends value

  trait inner extends Expression
  case class cons(x: value, inner: inner) extends inner
  case class nil() extends inner

  @Recursive(1, 0)
  def recursiveOneLevel(inner: inner, enclosing: enclosing): inner = (inner, enclosing) match {
    case (a, outer(b)) => b
  }

  @Recursive(0, 0, 0)
  def recursiveTwoLevel(outer2: outer2): inner = outer2 match {
    case outerouter(outer(cons(x, rest))) => rest
  }

}
