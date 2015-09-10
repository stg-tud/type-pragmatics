package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable

case class ConstructorDecl(name: String, in: Seq[SortRef], out: SortRef) extends VeritasConstruct with PrettyPrintable {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(name, " : ")
    in foreach (writer.write(_).write(" "))
    if (!in.isEmpty) writer.write("-> ")
    writer.write(out)
  }
}

object ConstructorDecl {
  def from(term: StrategoTerm): ConstructorDecl = term match {
    case StrategoAppl("ConstructorDecl", StrategoString(name), StrategoList(sortsIn), sortOut) => {
      ConstructorDecl(name, sortsIn map SortRef.from, SortRef.from(sortOut))
    }
  }
}
