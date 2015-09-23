package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable

case class FunctionEq(functionName: String, patterns: Seq[FunctionPattern], right: FunctionExp) extends VeritasConstruct with PrettyPrintable {
  override val children = Seq(patterns, Seq(right))

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (newchildren.length != 2 || newchildren(1).length != 1) 
      throw new ClassCastException
    try {
      val newpats: Seq[FunctionPattern] = newchildren(0) map {
        case p: FunctionPattern => p
        case _                  => throw new ClassCastException
      }
      val newright: FunctionExp = newchildren(1).head match {
        case e: FunctionExp => e
        case _              => throw new ClassCastException
      }
      FunctionEq(functionName, newpats, newright)
    } catch {
      case e: NoSuchElementException => throw new ClassCastException //happens if newchildren empty!
      case e: Exception              => throw e //should not happen!
    }
  }

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
