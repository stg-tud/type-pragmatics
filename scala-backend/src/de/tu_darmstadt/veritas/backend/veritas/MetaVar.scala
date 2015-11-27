package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoString

class MetaVar(_name: String) extends VeritasConstruct with SimplePrettyPrintable {
  val name = _name
  override val children = Seq()

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (!newchildren.isEmpty) throw new ClassCastException

    //return myself
    MetaVar(name)
  }

  override def prettyString = s"~$name"
  override def toString() = s"~${name}"
}

object MetaVar {
  def apply(name: String) = new MetaVar(name)
  def unapply(v: MetaVar) = Some(v.name)
  
  def from(term: StrategoTerm): MetaVar = term match {
    case StrategoAppl("Meta", StrategoString(varname)) => MetaVar(varname)
    case t => throw VeritasParseError("expected Meta() var, got: " + t)
  }
}