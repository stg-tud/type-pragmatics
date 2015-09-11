package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable

case class FunctionSig(name: String, in: Seq[SortRef], out: SortRef) extends VeritasConstruct with PrettyPrintable {
  override val children = Seq(in, Seq(out))

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (newchildren.length != 2 || newchildren(1).length != 1)
      throw new ClassCastException

    val newin: Seq[SortRef] = newchildren(0) map {
      case sr: SortRef => sr
      case _           => throw new ClassCastException
    }
    val newout: SortRef = newchildren(1).head match {
      case sr: SortRef => sr
      case _           => throw new ClassCastException
    }
    FunctionSig(name, newin, newout)
  }

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
