package de.tu_darmstadt.veritas.scalaspl.translatorfiles

import de.tu_darmstadt.veritas.scalaspl.lang.ScalaSPLSpecification

object ADTCorrect extends ScalaSPLSpecification {
  trait First extends Expression

  sealed trait Num extends Expression
  case class zero() extends Num
  case class succ(n: Num) extends Num
  case class succ2(n: Num, f: First) extends Num

  trait OtherNum extends Expression
  case class otherzero() extends OtherNum
  case class othersucc(n: Num) extends OtherNum
  case class othersucc2(n: OtherNum, f: First) extends OtherNum

}


