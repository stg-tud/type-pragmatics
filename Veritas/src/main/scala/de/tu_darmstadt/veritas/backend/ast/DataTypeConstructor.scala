package de.tu_darmstadt.veritas.backend.ast

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable

case class DataTypeConstructor(name: String, in: Seq[SortRef]) extends VeritasConstruct with PrettyPrintable {
  override val children = Seq(in)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(name, "(")
    if (!in.isEmpty) {
      writer.write(in.head)
      in.tail foreach { s =>
        writer.write(", ")
        writer.write(s)
      }
    }
    writer.write(")")
  }

  override def toString() = s"$name(${in.mkString(", ")})"
}

object DataTypeConstructor {
  def from(term: StrategoTerm): DataTypeConstructor = term match {
    case StrategoAppl("DataTypeConstructor", StrategoString(name), StrategoList(sortsIn)) => {
      DataTypeConstructor(name, sortsIn map SortRef.from)
    }
  }
}
