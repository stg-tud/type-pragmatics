package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable

case class SortDef(name: String) extends SimplePrettyPrintable {
  override protected val prettyString = name
}

case class SortRef(name: String) extends SimplePrettyPrintable {
  override protected val prettyString = name
}

object SortDef {
  def from(term: StrategoTerm): SortDef = term match {
    case StrategoAppl("SortDef", StrategoString(name)) => SortDef(name)
  }
}

object SortRef {
  def from(term: StrategoTerm): SortRef = term match {
    case StrategoAppl("SortRef", StrategoString(name)) => SortRef(name)
  }
}
