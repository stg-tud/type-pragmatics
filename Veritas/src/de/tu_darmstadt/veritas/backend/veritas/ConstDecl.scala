package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable

case class ConstDecl(name: String, out: SortRef) extends VeritasConstruct with PrettyPrintable {
  override val children = Seq(Seq(out))

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
