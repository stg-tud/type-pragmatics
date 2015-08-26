package de.tu_darmstadt.veritas.backend.tff

import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable
import de.tu_darmstadt.veritas.backend.fof.Variable

final case class TypedVariable(name: String, tfftype: TffAtomicType) extends Variable(name) with SimplePrettyPrintable {
  override val prettyString = super.toPrettyString() + " : " + tfftype.toPrettyString()
  // TODO add pretty String for TffType
}