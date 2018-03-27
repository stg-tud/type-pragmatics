package de.tu_darmstadt.veritas.backend.smtlib

import de.tu_darmstadt.veritas.backend.util.prettyprint.{PrettyPrintWriter, PrettyPrintableFile}

/**
  * Created by andiderp on 03.07.17.
  */
// TODO: do we need a SMTLIbAnnotated class?
case class SMTLibFile(filename: String, goalname: String, content: Seq[SMTLib]) extends PrettyPrintableFile {
  override def prettyPrint(writer: PrettyPrintWriter) = content foreach (writer.writeln(_))
}
