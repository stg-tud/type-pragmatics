package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable

sealed trait FunctionPattern extends PrettyPrintable

case class FunctionPatApp(functionName: String, args: Seq[FunctionPattern]) extends FunctionPattern {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(functionName, "(")
    args.dropRight(1) foreach (writer.write(_).write(", "))
    args.lastOption foreach (writer.write(_))
    writer.write(")")
  }
}

case class FunctionPatVar(varName: String) extends FunctionPattern with SimplePrettyPrintable {
  override val prettyString = varName
}

object FunctionPattern {
  def from(term: StrategoTerm): FunctionPattern = term match {
    case StrategoAppl("FunctionPatApp", StrategoString(func), StrategoList(args)) 
      => FunctionPatApp(func, args map FunctionPattern.from)
    case StrategoAppl("FunctionPatVar", StrategoString(v)) => FunctionPatVar(v)
    case t => throw VeritasParseError(t)
  }
}
