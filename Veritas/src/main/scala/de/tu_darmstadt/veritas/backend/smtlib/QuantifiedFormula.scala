package de.tu_darmstadt.veritas.backend.smtlib

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

/**
  * Created by andiderp on 03.07.17.
  */
sealed abstract class QuantifiedFormula(variableList: Seq[SortedVariable],
                                        formula: Term,
                                        opString: String,
                                        className: String) extends Term {
  require(!variableList.isEmpty, "quantify at least over 1 variable")

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(s"($opString ((")
    writer.write(variableList.head)
    writer.write(")")
    variableList.tail foreach { v =>
      writer.write(" (")
      writer.write(v)
      writer.write(")")
    }
    writer.write(") ")
    writer.write(formula)
    writer.write(")")
  }

  override def toString = className + variableList.mkString("(vars: ", ", ", ")")
}


final class ForAll private(val variableList: Seq[SortedVariable], val formula: Term)
  extends QuantifiedFormula(variableList, formula, "forall", "ForAll")

object ForAll {
  def apply(variableList: Seq[SortedVariable], formula: Term): Term = variableList match {
    case Seq() => formula
    case _ => new ForAll(variableList, formula)
  }
}


final class Exists private(val variableList: Seq[SortedVariable], val formula: Term)
  extends QuantifiedFormula(variableList, formula, "exists", "Exists")

object Exists {
  def apply(variableList: Seq[SortedVariable], formula: Term): Term = variableList match {
    case Seq() => formula
    case _ => new Exists(variableList, formula)
  }
}
