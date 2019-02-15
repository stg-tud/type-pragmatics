package de.tu_darmstadt.veritas.scalaspl

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification


// specification of typed arithmetic expressions as given in Pierce, TAPL, Chapters 3 and 8
// added a Plus operation
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

  case class Plus(t1: Term, t2: Term) extends Term

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

  @Partial
  @Dynamic
  @Recursive(1)
  @PreservationProperty("PlusPreservation")
  def plusop(t: Term, t1: Term): Term = (t, t1) match {
    case (t2, Zero()) => t2
    case (t2, Succ(t3)) => Succ(plusop(t2, t3))
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
    case Plus(t1, t2) =>
      if (isNV(t1) && isNV(t2))
        someTerm(plusop(t1, t2))
      else {
        val ot1 = reduce(t1)
        if (isSomeTerm(ot1))
          someTerm(Plus(getTerm(ot1), t2))
        else {
          val ot2 = reduce(t2)
          if (isSomeTerm(ot2))
            someTerm(Plus(t1, getTerm(ot2)))
          else
            noTerm()
        }
      }
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
  def Tfalse(): Unit = {} ensuring (False() :: B())

  @Axiom
  def Tif(t1: Term, t2: Term, t3: Term, T: Ty): Unit = {
    require(t1 :: B())
    require(t2 :: T)
    require(t3 :: T)
  } ensuring (Ifelse(t1, t2, t3) :: T)

  //inversion axiom for Ifelse
  //@Axiom
  //def Tif_inv(t1: Term, t2: Term, t3: Term, T: Ty): Unit = {
  //  require(Ifelse(t2, t2, t3) :: T)
  //} ensuring((t1 :: B()) && (t2 :: T) && (t3 :: T))

  //inversion axioms for Ifelse, since the above version causes translation problems:
  @Axiom
  def Tif_inv1(t1: Term, t2: Term, t3: Term, T: Ty): Unit = {
    require(Ifelse(t1, t2, t3) :: T)
  } ensuring (t1 :: B())

  @Axiom
  def Tif_inv2(t1: Term, t2: Term, t3: Term, T: Ty): Unit = {
    require(Ifelse(t1, t2, t3) :: T)
  } ensuring (t2 :: T)

  @Axiom
  def Tif_inv3(t1: Term, t2: Term, t3: Term, T: Ty): Unit = {
    require(Ifelse(t1, t2, t3) :: T)
  } ensuring (t3 :: T)


  @Axiom
  def TNat(): Unit = {} ensuring (Zero() :: Nat())

  @Axiom
  def TSucc(t1: Term): Unit = {
    require(t1 :: Nat())
  } ensuring (Succ(t1) :: Nat())

  //inversion axiom for TSucc
  @Axiom
  def TSucc_inc(t1: Term): Unit = {
    require(Succ(t1) :: Nat())
  } ensuring (t1 :: Nat())

  @Axiom
  def TPred(t1: Term): Unit = {
    require(t1 :: Nat())
  } ensuring (Pred(t1) :: Nat())

  //inversion axiom for TPred
  @Axiom
  def TPred_inv(t1: Term): Unit = {
    require(Pred(t1) :: Nat())
  } ensuring (t1 :: Nat())

  @Axiom
  def Tiszero(t1: Term): Unit = {
    require(t1 :: Nat())
  } ensuring (Iszero(t1) :: B())

  //inversion axiom for Tiszero
  @Axiom
  def Tiszero_inv(t1: Term): Unit = {
    require(Iszero(t1) :: B())
  } ensuring (t1 :: Nat())

  @Axiom
  def TPlus(t1: Term, t2: Term): Unit = {
    require(t1 :: Nat())
    require(t2 :: Nat())
  } ensuring(Plus(t1, t2) :: Nat())

  //inversion axioms for TPlus
  @Axiom
  def TPlus_inv1(t1: Term, t2: Term): Unit = {
    require(Plus(t1, t2) :: Nat())
  } ensuring(t1 :: Nat())


  @Axiom
  def TPlus_inv2(t1: Term, t2: Term): Unit = {
    require(Plus(t1, t2) :: Nat())
  } ensuring(t2 :: Nat())

  // steps for soundness proof (progress and preservation) for typed arithmetic expressions as given in Pierce, TAPL, Chapter 8
  @Property
  def Progress(t: Term, T: Ty): Unit = {
    require(t :: T)
    require(!isValue(t))
  } ensuring exists((t2: Term) => reduce(t) == someTerm(t2))

  @Property
  def Preservation(t: Term, T: Ty, t2: Term): Unit = {
    require(t :: T)
    require(reduce(t) == someTerm(t2))
  } ensuring (t2 :: T)

  @Property
  def PlusPreservation(t: Term, t1: Term): Unit = {
    require(t :: Nat())
    require(t1 :: Nat())
  } ensuring(plusop(t, t1) :: Nat())

}
