package de.tu_darmstadt.veritas.backend.veritas.function

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.veritas._

case class FunctionEq(functionName: String, patterns: Seq[FunctionPattern], right: FunctionExp) extends VeritasConstruct with PrettyPrintable {
  override val children = Seq(patterns, Seq(right))

  override def prettyPrint(writer: PrettyPrintWriter) = {
    // TODO print the parenthesis if patterns.isEmpty?
    writer.write(functionName, "(")
    patterns.dropRight(1) foreach (writer.write(_).write(", "))
    patterns.lastOption foreach (writer.write(_))
    writer.write(") = ")
    writer.indentOptional().write(right).unindent()
  }
  
  override def toString() = s"${functionName}(${patterns.mkString(",")}) = ${right}"
}

object FunctionEq {
  def from(term: StrategoTerm): FunctionEq = term match {
    case StrategoAppl("FunctionEq", StrategoString(func), StrategoList(patterns), right) => FunctionEq(func, patterns map FunctionPattern.from, FunctionExp.from(right))
  }
}
