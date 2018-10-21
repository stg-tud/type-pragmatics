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
  case class vnat(n: Nat) extends Value
  case class vtrue() extends Value
  case class vfalse() extends Value

  sealed trait Expr extends Expression
  case class evalue(v: Value) extends Expr
  case class eplus(left: Expr, right: Expr) extends Expr
  case class egt(left: Expr, right: Expr) extends Expr

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
    case evalue(_) => true
    case eplus(_, _) => false
    case egt(_, _) => false
  }

  @ProgressProperty("Progress")
  def reduce(e: Expr): OptExpr = e match {
    case evalue(vnat(_)) =>
      noExpr()
    case evalue(vtrue()) =>
      noExpr()
    case evalue(vfalse()) =>
      noExpr()
    case eplus(evalue(vnat(leftN)), evalue(vnat(rightN))) =>
      someExpr(evalue(vnat(add(leftN, rightN))))
    case eplus(evalue(vnat(leftN)), rightExp) =>
      val rightResult = reduce(rightExp)
      if(isSomeExpr(rightResult))
        someExpr(eplus(evalue(vnat(leftN)), getExpr(rightResult)))
      else
        noExpr()
    case eplus(leftExp, rightExp) =>
      val leftResult = reduce(leftExp)
      if(isSomeExpr(leftResult))
        someExpr(eplus(getExpr(leftResult), rightExp))
      else
        noExpr()
    case egt(evalue(vnat(leftN)), evalue(vnat(rightN))) =>
      if(gt(leftN, rightN))
        someExpr(evalue(vtrue()))
      else
        someExpr(evalue(vfalse()))
    case egt(evalue(vnat(leftN)), rightExp) =>
      val rightResult = reduce(rightExp)
      if(isSomeExpr(rightResult))
        someExpr(egt(evalue(vnat(leftN)), getExpr(rightResult)))
      else
        noExpr()
    case egt(leftExp, rightExp) =>
      val leftResult = reduce(leftExp)
      if(isSomeExpr(leftResult))
        someExpr(egt(getExpr(leftResult), rightExp))
      else
        noExpr()
  }

  @Axiom
  def TTrue(): Unit = {
  } ensuring (evalue(vtrue()) :: TBool())

  @Axiom
  def TFalse(): Unit = {
  } ensuring (evalue(vfalse()) :: TBool())

  @Axiom
  def TNatural(x: Nat): Unit = {
  } ensuring (evalue(vnat(x)) :: TNat())

  @Axiom
  def TPlus(left: Expr, right: Expr): Unit = {
    require(left :: TNat())
    require(right :: TNat())
  } ensuring (eplus(left, right) :: TNat())

  @Axiom
  def TGreater(left: Expr, right: Expr): Unit = {
    require(left :: TNat())
    require(right :: TNat())
  } ensuring (egt(left, right) :: TNat())

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
