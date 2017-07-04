package de.tu_darmstadt.veritas.backend.smtlib

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

/**
  * Created by andiderp on 04.07.17.
  */
case class Assertion(content: SMTLib) extends SMTLib {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write("(assert ")
    writer.write(content)
    writer.write(")")
  }
}
