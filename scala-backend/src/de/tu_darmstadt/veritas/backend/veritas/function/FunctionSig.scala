package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable

case class FunctionSig(name: String, in: Seq[SortRef], out: SortRef) extends VeritasConstruct with PrettyPrintable {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(name, " : ")
    in foreach (writer.write(_).write(" "))
    if (!in.isEmpty) writer.write("-> ")
    writer.write(out)
  }
}

object FunctionSig {
  def from(term: StrategoTerm): FunctionSig = term match {
    case StrategoAppl("FunctionSig", StrategoString(name), StrategoList(sortsIn), sortOut) => {
      FunctionSig(name, sortsIn map SortRef.from, SortRef.from(sortOut))
    }
  }
}
