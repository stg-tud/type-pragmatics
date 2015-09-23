package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable

case class SortDef(name: String) extends VeritasConstruct with SimplePrettyPrintable {
  require(!(SortDef.predefinedSorts contains name), "Attempting to redefine a predefined sort!")
  override val children = Seq()

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (!newchildren.isEmpty) throw new ClassCastException

    //return myself
    SortDef(name)
  }
  
  override def prettyString = name
  override def toString() = name
}

case class SortRef(name: String) extends VeritasConstruct with SimplePrettyPrintable {
  override val children = Seq()

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (!newchildren.isEmpty) throw new ClassCastException

    //return myself
    SortRef(name)
  }
  
  override def prettyString = name
  override def toString() = name
}

object SortDef {
  
  val predefinedSorts: List[String] = List("Bool", "iType")
  
  def from(term: StrategoTerm): SortDef = term match {
    case StrategoAppl("SortDef", StrategoString(name)) => SortDef(name)
  }
}

object SortRef {
  def from(term: StrategoTerm): SortRef = term match {
    case StrategoAppl("SortRef", StrategoString(name)) => SortRef(name)
  }
}
