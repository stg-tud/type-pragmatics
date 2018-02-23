package de.tu_darmstadt.veritas.newinputdsl.translatorfiles

import de.tu_darmstadt.veritas.newinputdsl.lang.SPLSpecification

object ADTCorrect extends SPLSpecification {
  override def typable(context: Context, exp: Expression, typ: Typ) = true

  override def typable(exp: Expression, typ: Typ) = true

  @Open
  trait First extends Expression

  trait Num extends Expression
  case class zero() extends Num
  case class succ(n: Num) extends Num
  case class succ2(n: Num, f: First) extends Num

  @Open
  trait OtherNum extends Expression
  case class otherzero() extends OtherNum
  case class othersucc(n: Num) extends OtherNum
  case class othersucc2(n: OtherNum, f: First) extends OtherNum

}


