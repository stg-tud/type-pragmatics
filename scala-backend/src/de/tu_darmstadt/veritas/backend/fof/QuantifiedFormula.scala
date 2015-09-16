package de.tu_darmstadt.veritas.backend.fof

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

sealed abstract class QuantifiedFormula(variableList: Seq[Variable],
                                        formula: FofUnitary,
                                        opString: String,
                                        className: String) extends FofUnitary {
  require(!variableList.isEmpty, "quantify at least over 1 variable")

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(opString + "[")
    writer.write(variableList.head)
    // NOTE see ScalaUnderscoreVsFunctionLiteral
    variableList.tail foreach { v =>
      writer.write(", ")
      writer.write(v)
    }
    writer.write("] : ")
    writer.write(formula)
  }
  
  override def toString = className + variableList.mkString("(vars: ", ", ", ")")
}


final class ForAll private(val variableList: Seq[Variable], val formula: FofUnitary) 
  extends QuantifiedFormula(variableList, formula, "!", "ForAll")

object ForAll {
  def apply(variableList: Seq[Variable], formula: FofUnitary): FofUnitary = variableList match {
    case Seq() => formula
    case _ => new ForAll(variableList, formula)
  }
}


final class Exists private(val variableList: Seq[Variable], val formula: FofUnitary)
  extends QuantifiedFormula(variableList, formula, "?", "Exists")

object Exists {
  def apply(variableList: Seq[Variable], formula: FofUnitary): FofUnitary = variableList match {
    case Seq() => formula
    case _ => new Exists(variableList, formula)
  }
}
