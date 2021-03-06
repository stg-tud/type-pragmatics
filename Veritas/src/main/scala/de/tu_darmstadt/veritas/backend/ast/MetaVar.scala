package de.tu_darmstadt.veritas.backend.ast

import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.transformation.collect.Typeable

case class MetaVar(name: String) extends VeritasConstruct with SimplePrettyPrintable with Typeable {
  override val children = Seq()

  override def prettyString = s"~$name"
  override def toString() = s"~${name}"
}

object MetaVar {
  def from(term: StrategoTerm): MetaVar = term match {
    case StrategoAppl("Meta", StrategoString(varname)) => MetaVar(varname)
    case t => throw VeritasParseError("expected Meta() var, got: " + t)
  }
}