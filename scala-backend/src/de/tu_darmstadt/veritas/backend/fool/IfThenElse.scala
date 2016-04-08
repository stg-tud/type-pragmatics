package de.tu_darmstadt.veritas.backend.fool

import de.tu_darmstadt.veritas.backend.fof.Term
import de.tu_darmstadt.veritas.backend.fof.FofUnitary
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

final case class IfThenElse(g: FofUnitary, t: Term, e: Term) extends Term {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("$ite(")
    writer.write(g).write(", ")
    writer.write(t).write(", ")
    writer.write(e).write(")")
  }
  
  override def toString = s"IfThenElse($g, $t, $e)"

}

