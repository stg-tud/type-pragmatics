package de.tu_darmstadt.veritas.newinputdsl

object LocalCorrect extends SPLSpecification {
  override def typable(context: LocalCorrect.Context, exp: LocalCorrect.Expression, typ: LocalCorrect.Typ) = true
  override def typable(exp: LocalCorrect.Expression, typ: LocalCorrect.Typ) = true
  trait Num extends Expression
  case class zero() extends Num
  case class succ(n: Num) extends Num

  @Local
  trait X {
    val a: Num
    val b: Num
    @Different
    val c: Num
    val d: Num

    @Axiom
    def non(): Unit = {
    } ensuring (true)

    val e: Num
    val f: Num
  }

}
