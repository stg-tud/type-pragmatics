package de.tu_darmstadt.veritas.scalaspl.translatorfiles

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification

object AxiomCorrect extends ScalaSPLSpecification {
  sealed trait Num extends Expression
  case class zero() extends Num
  case class succ(n: Num) extends Num

  sealed trait C extends Context
  case class cempty() extends C

  sealed trait T extends Type
  case class atyp() extends T

  @Axiom
  def simple(): Unit = {
    require(false)
    require(true)
  } ensuring (
    forall((x: Num) => succ(x) == zero())
  ) ensuring (
      true
  )

  @Axiom
  def orcase(): Unit = {
  } ensuring (((zero() == zero()) & (zero() == succ(zero()))) || (zero() == zero()))

  @Axiom
  def multipleconcls(): Unit = {
  } ensuring (zero() == zero()) ensuring (zero() == succ(zero())) ensuring (true)

  @Lemma
  def metavariables(z: Num, a: Num): Unit = {
    require(true)
    require(exists((x: Num, y: Num) => (x != y) & (a == x)))
  }  ensuring (forall((x: Num) => succ(x) == z))

  @Goal
  def typing(z: Num, a: Num): Unit = {
    require(z :: atyp())
    require(cempty() |- a :: atyp())
  }  ensuring (true)
}
