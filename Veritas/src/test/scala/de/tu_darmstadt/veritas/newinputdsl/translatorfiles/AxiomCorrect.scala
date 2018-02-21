package de.tu_darmstadt.veritas.newinputdsl.translatorfiles

import de.tu_darmstadt.veritas.newinputdsl.SPLSpecification

object AxiomCorrect extends SPLSpecification {
  override def typable(context: AxiomCorrect.Context, exp: AxiomCorrect.Expression, typ: AxiomCorrect.Typ) = true

  override def typable(exp: AxiomCorrect.Expression, typ: AxiomCorrect.Typ) = true

  trait Num extends Expression
  case class zero() extends Num
  case class succ(n: Num) extends Num

  trait C extends Context
  case class cempty() extends C

  trait T extends Typ
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
  } ensuring (((zero() == zero()) $$ (zero() == succ(zero()))) || (zero() == zero()))

  @Lemma
  def metavariables(z: Num, a: Num): Unit = {
    require(true)
    require(exists((x: Num, y: Num) => (x != y) $$ (a == x)))
  }  ensuring (forall((x: Num) => succ(x) == z))

  @Goal
  def typing(z: Num, a: Num): Unit = {
    require(z :: atyp())
    require(cempty() |- a :: atyp())
  }  ensuring (true)
}
