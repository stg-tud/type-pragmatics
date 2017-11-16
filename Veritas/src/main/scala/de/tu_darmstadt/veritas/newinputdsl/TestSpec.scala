package de.tu_darmstadt.veritas.newinputdsl

import scala.annotation.StaticAnnotation

trait SPLSpecification {
  case class Open() extends StaticAnnotation
  case class Different() extends StaticAnnotation
  case class Axiom() extends StaticAnnotation
  case class Lemma() extends StaticAnnotation
  case class Goal() extends StaticAnnotation
  case class Local() extends StaticAnnotation

  case class StaticDomain() extends StaticAnnotation
  case class DynamicDomain() extends StaticAnnotation
  case class SimpleRecursive() extends StaticAnnotation
  case class Transform() extends StaticAnnotation

  case class LemmaWith(name: String) extends StaticAnnotation
  case class GoalWith(name: String) extends StaticAnnotation
  case class HideAll() extends StaticAnnotation
  case class Hide() extends StaticAnnotation
  case class Strategy() extends StaticAnnotation

  // shortcomings: is not executable. But we dont even know yet what it means to have exectuable axioms / lemmas
  def forall[T1](fun: Function1[T1, Boolean]): Boolean = true
  def forall[T1, T2](fun: Function2[T1, T2, Boolean]): Boolean = true
  def forall[T1, T2, T3](fun: Function3[T1, T2, T3, Boolean]): Boolean = true
  def forall[T1, T2, T3, T4](fun: Function4[T1, T2, T3, T4, Boolean]): Boolean = true
  def forall[T1, T2, T3, T4, T5](fun: Function5[T1, T2, T3, T4, T5, Boolean]): Boolean = true
  def forall[T1, T2, T3, T4, T5, T6](fun: Function6[T1, T2, T3, T4, T5, T6, Boolean]): Boolean = true
  def forall[T1, T2, T3, T4, T5, T6, T7](fun: Function7[T1, T2, T3, T4, T5, T6, T7, Boolean]): Boolean = true

  def exists[T1](fun: Function1[T1, Boolean]): Boolean = true
  def exists[T1, T2](fun: Function2[T1, T2, Boolean]): Boolean = true
  def exists[T1, T2, T3](fun: Function3[T1, T2, T3, Boolean]): Boolean = true
  def exists[T1, T2, T3, T4](fun: Function4[T1, T2, T3, T4, Boolean]): Boolean = true
  def exists[T1, T2, T3, T4, T5](fun: Function5[T1, T2, T3, T4, T5, Boolean]): Boolean = true
  def exists[T1, T2, T3, T4, T5, T6](fun: Function6[T1, T2, T3, T4, T5, T6, Boolean]): Boolean = true
  def exists[T1, T2, T3, T4, T5, T6, T7](fun: Function7[T1, T2, T3, T4, T5, T6, T7, Boolean]): Boolean = true

  //let
  //typing
  // G |- e :: T

  // every expression has to be a subclass of this trait
  // Therefore we can can determine automatically which params belong to the expression domain
  trait Expression
  trait Context
  trait Typ
  def typable(context: Context, exp: Expression, typ: Typ): Boolean
}
// Because of scalameta we cannot easily know what all the superclasses of a class are.
// The first step is it to build up the type hierarchy and check if only acceptable types are used.

object Spec extends SPLSpecification {
  def typable(context: Context, exp: Expression, typ: Typ): Boolean = true
  trait char

  trait YN
  case class yes() extends YN
  case class no() extends YN

  def and(b1: YN, b2: YN): YN = (b1, b2) match {
    case (yes(), yes()) => yes()
    case (_, _) => no()
  }

  @SimpleRecursive
  def gt(a: nat, b: nat): YN = (a, b) match {
    case (zero(), _) => no()
    case (succ(_), zero()) => yes()
    case (succ(n1), succ(n2)) => gt(n1, n2)
  }

  // is not really transformed but wanted to test
  def minus(@Transform a: nat, b: nat): nat = (a, b) match {
    case (_, zero()) => a
    case (_, succ(b)) => pred(minus(a, b))
  }

  // if works
  def divide(a: nat, b: nat): nat = (a, b) match {
    case (_, _) =>
      if (gt(a , b) == yes())
        succ(divide(minus(a, b), b))
      else
        zero()
  }


  /*def intersectATM(atm1: ATMap, atm2: ATMap): ATMap =
  (atm1, atm2) match {
  case (atmbind(qid, at, at1), _) => {
  val atm1atm2 = intersectATM(at1, atm2)
  val lAT = lookupATM(qid, atm2)
  if (isSomeAType(lAT) && (getAType(lAT) == at))
  atmbind(qid, at, atm1atm2)
  else
  atm1atm2
  }
  case _ => ()
  }
  */

  trait nat
  case class zero() extends nat
  case class succ(n: nat) extends nat

  def pred(n: nat): nat = n match {
    case zero() => zero()
    case succ(n) => n
  }

  trait ATMap
  trait QID
  trait AType

  def counterexample2(atm: ATMap, qid1: QID, t: AType, atm2: ATMap) = {
    require(true)
    require(true)
  } ensuring (atm != null)

  @Local
  trait X {
    @Different
    val x: QID
    val y: QID

    @Goal
    def x(x: ATMap) = {
      require(true)
    } ensuring (
      // exp |- x :: exp
      forall((a: ATMap, b: QID) => a == b) && exists((a: ATMap) => true)
    )
  }


  @Strategy
  trait Y {
    @Local
    trait Sub {
      val x: QID

      @Axiom
      def ax = {
        require(true)
      } ensuring (true)

      @HideAll
      @Goal
      def goal = {
        require(true)
      } ensuring (true)
    }

    // what does hide all do?
    def y = {
    } ensuring(true)
  }

  @GoalWith("Y")
  def finalgoal = {
    require(true)
  } ensuring (true)
}
