package de.tu_darmstadt.veritas.backend.fof

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable

// NOTE I would like to make this sealed, but then PlainTerm would have to be in this file...
/* sealed */ trait FofUnitary extends Fof

final class Parenthesized private(val formula: Fof) extends FofUnitary {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("(")
    writer.write(formula).write(")")
  } 
  override def toString = s"($formula)"
}
object Parenthesized {
  def apply(formula: Fof): FofUnitary = formula match {
    case alreadyUnitary: FofUnitary => alreadyUnitary
    case _ => new Parenthesized(formula)
  }
  def unapply(e: Parenthesized): Option[Fof] = Some(e.formula)
}

final class Not private(val arg: FofUnitary) extends FofUnitary {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("!(")
    writer.write(arg).write(")")
  } 
  override def toString = s"Not($arg)"
}
object Not {
  def apply(arg: FofUnitary): FofUnitary = arg match {
    case True => False
    case False => True
    case _: Not => arg
    case _ => new Not(arg)
  }
  def unapply(e: Not): Option[FofUnitary] = Some(e.arg)
}

sealed abstract class FofInfixUnary(termLeft: Term,
                                    termRight: Term,
                                    opString: String) extends FofUnitary {

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(termLeft).write(opString)
    writer.write(termRight)
  }
}

final case class Eq(termLeft: Term, termRight: Term) 
  extends FofInfixUnary(termLeft, termRight, " = ")

final case class NeqEq(termLeft: Term, termRight: Term)
  extends FofInfixUnary(termLeft, termRight, " != ")

final case object True extends FofUnitary with SimplePrettyPrintable {
  override val prettyString = "$true"
}

final case object False extends FofUnitary with SimplePrettyPrintable {
  override val prettyString = "$false"
}
