package de.tu_darmstadt.veritas.backend.fof

import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

trait FormulaRole extends SimplePrettyPrintable

final case object Axiom extends FormulaRole {
  override val prettyString = "axiom"
}

final case object Conjecture extends FormulaRole {
  override val prettyString = "conjecture"
}
