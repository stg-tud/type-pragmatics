package de.tu_darmstadt.veritas.backend.tff

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.fof.FormulaRole
import de.tu_darmstadt.veritas.backend.fof.{Fof, Parenthesized}

/** 
 * A TFF File consists of one or more of these TffAnnotated.
 * TffAnnotated is basically == Typed First order Formula (tff) + Role (Conjecture or Axiom etc.)
 * All fof formulas are also tffs !
 */
final case class TffAnnotated(name: String, role: FormulaRole, formula: Fof) extends PrettyPrintable {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("tff('", name, "', ").write(role).write(", ")
    writer.write(Parenthesized(formula)).write(").")
  }
}

