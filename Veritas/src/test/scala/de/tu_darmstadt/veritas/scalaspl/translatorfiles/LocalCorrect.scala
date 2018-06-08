package de.tu_darmstadt.veritas.scalaspl.translatorfiles

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification

object LocalCorrect extends ScalaSPLSpecification {
  sealed trait Num extends Expression
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
