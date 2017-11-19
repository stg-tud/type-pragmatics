package de.tu_darmstadt.veritas.newinputdsl

object AxiomCorrect extends SPLSpecification {
  override def typable(context: AxiomCorrect.Context, exp: AxiomCorrect.Expression, typ: AxiomCorrect.Typ) = true

  override def typable(exp: AxiomCorrect.Expression, typ: AxiomCorrect.Typ) = true

  trait Num extends Expression
  case class zero() extends Num
  case class succ(n: Num) extends Num

  @Axiom
  def simple(): Unit = {
    require(true)
    require(exists((x: Num, y: Num) => x != y))
  }  ensuring (forall((x: Num) => succ(x) == zero()))

  @Axiom
  def metavariables(z: Num, a: Num): Unit = {
    require(true)
    require(exists((x: Num, y: Num) => x != y && a == x))
  }  ensuring (forall((x: Num) => succ(x) == z))
}
