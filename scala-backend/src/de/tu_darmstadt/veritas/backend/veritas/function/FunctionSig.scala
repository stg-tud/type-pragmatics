package de.tu_darmstadt.veritas.backend.veritas.function

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.veritas._

case class FunctionSig(name: String, in: Seq[SortRef], out: SortRef) extends VeritasConstruct with PrettyPrintable {
  override val children = Seq(in, Seq(out))

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(name, " : ")
    in foreach (writer.write(_).write(" "))
    if (!in.isEmpty) writer.write("-> ")
    writer.write(out)
  }
  
  override def toString() = s"${name} : ${in.mkString(" ")} -> ${out}"
}

object FunctionSig {
  def from(term: StrategoTerm): FunctionSig = term match {
    case StrategoAppl("FunctionSig", StrategoString(name), StrategoList(sortsIn), sortOut) => {
      FunctionSig(name, sortsIn map SortRef.from, SortRef.from(sortOut))
    }
  }
}
