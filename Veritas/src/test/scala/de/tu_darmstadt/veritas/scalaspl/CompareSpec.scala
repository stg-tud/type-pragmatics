package de.tu_darmstadt.veritas.scalaspl

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification

object CompareSpec extends ScalaSPLSpecification {
  // natural numbers
  sealed trait Nat extends Expression
  case class zero() extends Nat
  case class succ(n: Nat) extends Nat

  def add(left: Nat, right: Nat): Nat = (left, right) match {
    case (zero(), r) => r
    case (succ(leftPred), r) => succ(add(leftPred, r))
  }

  def gt(a: Nat, b: Nat): Boolean = (a, b) match {
    case (zero(), _) => false
    case (succ(_), zero()) => true
    case (succ(aa), succ(bb)) => gt(aa, bb)
  }

  sealed trait Value extends Expression
  case class VNat(n: Nat) extends Value
  case class VTrue() extends Value
  case class VFalse() extends Value

  sealed trait Term extends Expression
  case class Number(n: Nat) extends Term
  case class True() extends Term
  case class False() extends Term
  case class Plus(left: Term, right: Term) extends Term
  case class GreaterThan(left: Term, right: Term) extends Term

  sealed trait OptExpr extends Expression
  case class noExpr() extends OptExpr
  case class someExpr(e: Term) extends OptExpr

  def isSomeExpr(e: OptExpr): Boolean = e match {
    case noExpr() => false
    case someExpr(_) => true
  }

  @Partial
  def getExpr(e: OptExpr): Term = e match {
    case someExpr(ee) => ee
  }

  sealed trait EType extends Type
  case class TBool() extends EType
  case class TNat() extends EType

  def isValue(e: Term): Boolean = e match {
    case Number(_) => true
    case True() => true
    case False() => true
    case Plus(_, _) => false
    case GreaterThan(_, _) => false
  }

  @ProgressProperty("Progress")
  def reduce(e: Term): OptExpr = e match {
    case Number(_) =>
      noExpr()
    case True() =>
      noExpr()
    case False() =>
      noExpr()
    case Plus(Number(leftN), Number(rightN)) =>
      someExpr(Number(add(leftN, rightN)))
    case Plus(Number(leftN), rightExp) =>
      val rightResult = reduce(rightExp)
      if(isSomeExpr(rightResult))
        someExpr(Plus(Number(leftN), getExpr(rightResult)))
      else
        noExpr()
    case Plus(leftExp, rightExp) =>
      val leftResult = reduce(leftExp)
      if(isSomeExpr(leftResult))
        someExpr(Plus(getExpr(leftResult), rightExp))
      else
        noExpr()
    case GreaterThan(Number(leftN), Number(rightN)) =>
      if(gt(leftN, rightN))
        someExpr(True())
      else
        someExpr(False())
    case GreaterThan(Number(leftN), rightExp) =>
      val rightResult = reduce(rightExp)
      if(isSomeExpr(rightResult))
        someExpr(GreaterThan(Number(leftN), getExpr(rightResult)))
      else
        noExpr()
    case GreaterThan(leftExp, rightExp) =>
      val leftResult = reduce(leftExp)
      if(isSomeExpr(leftResult))
        someExpr(GreaterThan(getExpr(leftResult), rightExp))
      else
        noExpr()
  }

  @Axiom
  def TTrue(): Unit = {
  } ensuring True() :: TBool()

  @Axiom
  def TFalse(): Unit = {
  } ensuring False() :: TBool()

  @Axiom
  def TNatural(x: Nat): Unit = {
  } ensuring  Number(x) :: TNat()

  @Axiom
  def TPlus(left: Term, right: Term): Unit = {
    require(left :: TNat())
    require(right :: TNat())
  } ensuring (Plus(left, right) :: TNat())

  @Axiom
  def TGreater(left: Term, right: Term): Unit = {
    require(left :: TNat())
    require(right :: TNat())
  } ensuring (GreaterThan(left, right) :: TBool())

  @Property
  def Progress(e1: Term, T: EType): Unit = {
    require(e1 :: T)
    require(!isValue(e1))
  } ensuring exists((e2: Term) => reduce(e1) == someExpr(e2))

  @Property
  def Preservation(e1: Term, e2: Term, T: EType): Unit = {
    require(e1 :: T)
    require(reduce(e1) == someExpr(e2))
  } ensuring (e2 :: T)
}
