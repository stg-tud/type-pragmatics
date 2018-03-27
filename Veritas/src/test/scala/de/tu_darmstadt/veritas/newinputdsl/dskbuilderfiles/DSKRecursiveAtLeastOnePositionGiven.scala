package de.tu_darmstadt.veritas.newinputdsl.dskbuilderfiles

import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification

object DSKRecursiveAtLeastOnePositionGiven extends SPLSpecification {
  sealed trait enclosing extends Expression
  case class outer(inner: inner) extends enclosing

  sealed trait value extends Expression
  case class valuebind() extends value

  sealed trait inner extends Expression
  case class cons(x: value, inner: inner) extends inner
  case class nil() extends inner

  @Recursive()
  def recursiveOneLevel(inner: inner, enclosing: enclosing): inner = (inner, enclosing) match {
    case (a, outer(b)) => b
  }
}
