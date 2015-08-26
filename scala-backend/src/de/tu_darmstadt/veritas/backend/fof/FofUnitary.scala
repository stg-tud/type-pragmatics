package de.tu_darmstadt.veritas.backend.fof

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable

// NOTE I would like to make this sealed, but then PlainTerm would have to be in this file...
/* sealed */ trait FofUnitary extends Fof

final case class Parenthesized(val formula: Fof) extends FofUnitary {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("(")
    writer.write(formula).write(")")
  } 
}
final case class Not(arg: FofUnitary) extends FofUnitary {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("!(")
    writer.write(arg).write(")")
  } 
}


sealed abstract class QuantifiedFormula(variableList: Seq[Variable],
                                        formula: FofUnitary,
                                        opString: String) extends FofUnitary {
  require(!variableList.isEmpty, "quantify at least over 1 variable")

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("(" + opString + "[")
    writer.write(variableList.head)
    // NOTE see ScalaUnderscoreVsFunctionLiteral
    variableList.tail foreach { v =>
      writer.write(", ")
      writer.write(v)
    }
    writer.write("] : ")
    writer.write(formula)
    writer.write(")")
  }
}

final case class ForAll(variableList: Seq[Variable], formula: FofUnitary) 
  extends QuantifiedFormula(variableList, formula, "!")
final case class Exists(variableList: Seq[Variable], formula: FofUnitary)
  extends QuantifiedFormula(variableList, formula, "?")

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
