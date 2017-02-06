package de.tu_darmstadt.veritas.backend.tff

import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable
import de.tu_darmstadt.veritas.backend.fof.{Variable, UntypedVariable}

final case class TypedVariable(name: String, tfftype: TffAtomicType) extends Variable(name) {
  override def prettyString = super.prettyString + ": " + tfftype.prettyString
  def toUntyped = UntypedVariable(name)
}