package de.tu_darmstadt.veritas.backend.smtlib

import de.tu_darmstadt.veritas.backend.util.prettyprint.{PrettyPrintWriter, PrettyPrintable, SimplePrettyPrintable}

/**
  * Created by andiderp on 03.07.17.
  */
trait SMTLib extends PrettyPrintable

final case class Assertion(content: SMTLib) extends SMTLib {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write("(assert ")
    writer.write(content)
    writer.write(")")
  }
}

final case class Goal(name: String, assertion: Assertion) extends SMTLib {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = writer.write(assertion)
}

final object CheckSat extends SMTLib with SimplePrettyPrintable {
  override def prettyString: String = "(check-sat)"
}
