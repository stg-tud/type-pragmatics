package de.tu_darmstadt.veritas.scalaspl

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification

object ToySpec extends ScalaSPLSpecification {
  // natural numbers
  sealed trait Nat extends Expression
  case class zero() extends Nat
  case class succ(n: Nat) extends Nat

  def add(a: Nat, b: Nat): Nat = (a, b) match {
    case (zero(), bb) => bb
    case (succ(aa), bb) => succ(add(aa, bb))
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

  sealed trait Expr extends Expression
  case class EValue(v: Value) extends Expr
  case class EPlus(left: Expr, right: Expr) extends Expr
  case class EGreaterThan(left: Expr, right: Expr) extends Expr

  sealed trait OptExpr extends Expression
  case class noExpr() extends OptExpr
  case class someExpr(e: Expr) extends OptExpr

  def isSomeExpr(e: OptExpr): Boolean = e match {
    case noExpr() => false
    case someExpr(_) => true
  }

  @Partial
  def getExpr(e: OptExpr): Expr = e match {
    case someExpr(ee) => ee
  }

  sealed trait EType extends Type
  case class TBool() extends EType
  case class TNat() extends EType

  def isValue(e: Expr): Boolean = e match {
    case EValue(_) => true
    case EPlus(_, _) => false
    case EGreaterThan(_, _) => false
  }

  @ProgressProperty("Progress")
  def reduce(e: Expr): OptExpr = e match {
    case EValue(VNat(_)) =>
      noExpr()
    case EValue(VTrue()) =>
      noExpr()
    case EValue(VFalse()) =>
      noExpr()
    case EPlus(EValue(VNat(leftN)), EValue(VNat(rightN))) =>
      someExpr(EValue(VNat(add(leftN, rightN))))
    case EPlus(EValue(VNat(leftN)), rightExp) =>
      val rightResult = reduce(rightExp)
      if(isSomeExpr(rightResult))
        someExpr(EPlus(EValue(VNat(leftN)), getExpr(rightResult)))
      else
        noExpr()
    case EPlus(leftExp, rightExp) =>
      val leftResult = reduce(leftExp)
      if(isSomeExpr(leftResult))
        someExpr(EPlus(getExpr(leftResult), rightExp))
      else
        noExpr()
    case EGreaterThan(EValue(VNat(leftN)), EValue(VNat(rightN))) =>
      if(gt(leftN, rightN))
        someExpr(EValue(VTrue()))
      else
        someExpr(EValue(VFalse()))
    case EGreaterThan(EValue(VNat(leftN)), rightExp) =>
      val rightResult = reduce(rightExp)
      if(isSomeExpr(rightResult))
        someExpr(EGreaterThan(EValue(VNat(leftN)), getExpr(rightResult)))
      else
        noExpr()
    case EGreaterThan(leftExp, rightExp) =>
      val leftResult = reduce(leftExp)
      if(isSomeExpr(leftResult))
        someExpr(EGreaterThan(getExpr(leftResult), rightExp))
      else
        noExpr()
  }

  @Axiom
  def TTrue(): Unit = {
  } ensuring (EValue(VTrue()) :: TBool())

  @Axiom
  def TFalse(): Unit = {
  } ensuring (EValue(VFalse()) :: TBool())

  @Axiom
  def TNatural(x: Nat): Unit = {
  } ensuring (EValue(VNat(x)) :: TNat())

  @Axiom
  def TPlus(left: Expr, right: Expr): Unit = {
    require(left :: TNat())
    require(right :: TNat())
  } ensuring (EPlus(left, right) :: TNat())

  @Axiom
  def TGreater(left: Expr, right: Expr): Unit = {
    require(left :: TNat())
    require(right :: TNat())
  } ensuring (EGreaterThan(left, right) :: TBool())

  @Property
  def Progress(e1: Expr, T: EType): Unit = {
    require(e1 :: T)
    require(!isValue(e1))
  } ensuring exists((e2: Expr) => reduce(e1) == someExpr(e2))

  @Property
  def Preservation(e1: Expr, e2: Expr, T: EType): Unit = {
    require(e1 :: T)
    require(reduce(e1) == someExpr(e2))
  } ensuring (e2 :: T)
}
