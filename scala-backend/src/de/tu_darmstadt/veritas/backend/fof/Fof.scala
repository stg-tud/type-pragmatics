package de.tu_darmstadt.veritas.backend.fof

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

// As reference for the TPTP/FOF syntax, see http://www.cs.miami.edu/~tptp/TPTP/SyntaxBNF.html#fof_annotated

// For a better overview of all the different Fof subclasses, look at the inheritance diagrams 
// in the scaladoc.

/** 
 * A FOF File consists of one or more of these FofAnnotated.
 * FofAnnotated is basically == First order Forumula (Fof) + Role (Conjecture or Axiom etc.)
 */
final case class FofAnnotated(name: String, role: FormulaRole, formula: Fof) extends PrettyPrintable {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("fof('", name, "', ").write(role).write(", ")
    writer.write(formula).write(").")
  }
}

/**
 * All First order formula "operators" (productions) like And, Or, Not, etc. extend this trait. 
 */
trait Fof extends PrettyPrintable
