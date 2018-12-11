package de.tu_darmstadt.veritas.scalaspl

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification


// specification of typed arithmetic expressions as given in Pierce, TAPL, Chapters 3 and 8
object AESpec extends ScalaSPLSpecification {

  //simple Boolean and arithmetic expressions
  sealed trait Term extends Expression
  case class True() extends Term
  case class False() extends Term
  case class Ifelse(b: Term, t: Term, e: Term) extends Term
  case class Zero() extends Term
  case class Succ(p: Term) extends Term
  case class Pred(s: Term) extends Term
  case class Iszero(t: Term) extends Term

  def isNV(t: Term): Boolean = t match {
    case Zero() => true
    case Succ(nv) => isNV(nv)
    case _ => false
  }

  def isValue(t: Term): Boolean = t match {
    case True() => true
    case False() => true
    case t1 => isNV(t1)
  }

  @FailableType
  sealed trait OptTerm
  case class noTerm() extends OptTerm
  case class someTerm(t: Term) extends OptTerm

  def isSomeTerm(t: OptTerm): Boolean = t match {
    case noTerm() => false
    case someTerm(_) => true
  }

  @Partial
  def getTerm(ot: OptTerm): Term = ot match {
    case someTerm(t) => t
  }

  //reduction semantics for simple Boolean and arithmetic terms
  @ProgressProperty("Progress")
  @PreservationProperty("Preservation")
  @Recursive(0)
  @Dynamic
  def reduce(t: Term): OptTerm = t match {
    case Ifelse(True(), t2, t3) => someTerm(t2)
    case Ifelse(False(), t2, t3) => someTerm(t3)
    case Ifelse(t1, t2, t3) =>
      val ot1 = reduce(t1)
      if (isSomeTerm(ot1))
        someTerm(Ifelse(getTerm(ot1), t2, t3))
      else
        noTerm()
    case Succ(t1) =>
      val ot2 = reduce(t1)
      if (isSomeTerm(ot2))
        someTerm(Succ(getTerm(ot2)))
      else
        noTerm()
    case Pred(Zero()) => someTerm(Zero())
    case Pred(Succ(nv)) => if (isNV(nv)) someTerm(nv) else noTerm()
    case Pred(t1) =>
      val ot2 = reduce(t1)
      if (isSomeTerm(ot2))
        someTerm(Pred(getTerm(ot2)))
      else
        noTerm()
    case Iszero(Zero()) => someTerm(True())
    case Iszero(Succ(nv)) => if (isNV(nv)) someTerm(False()) else noTerm()
    case Iszero(t1) =>
      val ot2 = reduce(t1)
      if (isSomeTerm(ot2))
        someTerm(Iszero(getTerm(ot2)))
      else
        noTerm()
    case _ => noTerm()
  }

  //types (Bool and Nat)
  sealed trait Ty extends Type
  case class B() extends Ty
  case class Nat() extends Ty

  //typing rules
  @Axiom
  def Ttrue(): Unit = {} ensuring (True() :: B())

  @Axiom
  def Tfalse(): Unit = {} ensuring(False() :: B())

  @Axiom
  def Tif(t1: Term, t2: Term, t3: Term, T: Ty): Unit = {
    require(t1 :: B())
    require(t2 :: T)
    require(t3 :: T)
  } ensuring(Ifelse(t1, t2, t3) :: T)

  @Axiom
  def TNat(): Unit = {} ensuring(Zero() :: Nat())

  @Axiom
  def TSucc(t1: Term): Unit = {
    require(t1 :: Nat())
  } ensuring(Succ(t1) :: Nat())

  @Axiom
  def TPred(t1: Term): Unit = {
    require(t1 :: Nat())
  } ensuring(Pred(t1) :: Nat())

  @Axiom
  def Tiszero(t1: Term): Unit = {
    require(t1 :: Nat())
  } ensuring(Iszero(t1) :: B())


  // steps for soundness proof (progress and preservation) for typed arithmetic expressions as given in Pierce, TAPL, Chapter 8
  @Property
  def Progress(t1: Term, T: Ty): Unit = {
    require(t1 :: T)
    require(!isValue(t1))
  } ensuring exists( (t2: Term) => reduce(t1) == someTerm(t2))

  @Property
  def Preservation(t1: Term, T: Ty, t2: Term): Unit = {
    require(t1 :: T)
    require(reduce(t1) == someTerm(t2))
  } ensuring(t2 :: T)

}
