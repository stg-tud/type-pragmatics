package de.tu_darmstadt.veritas.backend.tff

import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable
import de.tu_darmstadt.veritas.backend.fof.Variable

final case class TypedVariable(name: String, tfftype: TffAtomicType) extends Variable(name) with SimplePrettyPrintable {
  override def prettyString = super.prettyString + ": " + tfftype.prettyString
  // TODO add pretty String for TffType
}