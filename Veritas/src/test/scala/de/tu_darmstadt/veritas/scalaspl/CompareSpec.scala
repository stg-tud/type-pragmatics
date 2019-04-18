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

  @FailableType
  sealed trait OptTerm extends Expression
  case class noTerm() extends OptTerm
  case class someTerm(e: Term) extends OptTerm

  def isSomeTerm(e: OptTerm): Boolean = e match {
    case noTerm() => false
    case someTerm(_) => true
  }

  @Partial
  def getTerm(e: OptTerm): Term = e match {
    case someTerm(ee) => ee
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
  def reduce(e: Term): OptTerm = e match {
    case Number(_) =>
      noTerm()
    case True() =>
      noTerm()
    case False() =>
      noTerm()
    case Plus(Number(leftN), Number(rightN)) =>
      someTerm(Number(add(leftN, rightN)))
    case Plus(Number(leftN), rightExp) =>
      val rightResult = reduce(rightExp)
      if(isSomeTerm(rightResult))
        someTerm(Plus(Number(leftN), getTerm(rightResult)))
      else
        noTerm()
    case Plus(leftExp, rightExp) =>
      val leftResult = reduce(leftExp)
      if(isSomeTerm(leftResult))
        someTerm(Plus(getTerm(leftResult), rightExp))
      else
        noTerm()
    case GreaterThan(Number(leftN), Number(rightN)) =>
      if(gt(leftN, rightN))
        someTerm(True())
      else
        someTerm(False())
    case GreaterThan(Number(leftN), rightExp) =>
      val rightResult = reduce(rightExp)
      if(isSomeTerm(rightResult))
        someTerm(GreaterThan(Number(leftN), getTerm(rightResult)))
      else
        noTerm()
    case GreaterThan(leftExp, rightExp) =>
      val leftResult = reduce(leftExp)
      if(isSomeTerm(leftResult))
        someTerm(GreaterThan(getTerm(leftResult), rightExp))
      else
        noTerm()
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
  } ensuring exists((e2: Term) => reduce(e1) == someTerm(e2))

  @Property
  def Preservation(e1: Term, e2: Term, T: EType): Unit = {
    require(e1 :: T)
    require(reduce(e1) == someTerm(e2))
  } ensuring (e2 :: T)
}
