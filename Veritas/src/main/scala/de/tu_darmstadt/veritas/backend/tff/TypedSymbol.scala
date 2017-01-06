package de.tu_darmstadt.veritas.backend.tff

import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable
import de.tu_darmstadt.veritas.backend.fof.{FunSymbol, UntypedFunSymbol}
import de.tu_darmstadt.veritas.backend.fof.FofUnitary

final case class TypedSymbol(name: String, tfftype: TffTopLevelType) extends FunSymbol(name) 
  with FofUnitary {
  override def prettyString = super.prettyString + ": " + tfftype.prettyString
  def toUntyped = UntypedFunSymbol(name)
}

