package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable

case class ConstDecl(name: String, out: SortRef) extends VeritasConstruct with PrettyPrintable {
  override val children = Seq(Seq(out))

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (newchildren.length != 2 || newchildren(1).length != 1)
      throw new ClassCastException

    val newout: SortRef = newchildren(1).head match {
      case sr: SortRef => sr
      case _           => throw new ClassCastException
    }
    ConstDecl(name, newout)
  }

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(name, " : ")
    out.prettyPrint(writer)
  }
  
  override def toString() = s"${name} : ${out}"
}

object ConstDecl {
  def from(term: StrategoTerm): ConstDecl = term match {
    case StrategoAppl("ConstDecl", StrategoString(name), sortOut) => {
      ConstDecl(name, SortRef.from(sortOut))
    }
  }
}
