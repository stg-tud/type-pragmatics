package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoString

case class MetaVar(name: String) extends SimplePrettyPrintable {
  override protected val prettyString = s"~$name"
}

object MetaVar {
  def from(term: StrategoTerm): MetaVar = term match {
    case StrategoAppl("Meta", StrategoString(varname)) => MetaVar(varname)
    case t => throw VeritasParseError("expected Meta() var, got: " + t)
  }
}