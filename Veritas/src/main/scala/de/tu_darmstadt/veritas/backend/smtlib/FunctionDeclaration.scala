package de.tu_darmstadt.veritas.backend.smtlib

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

/**
  * Created by andiderp on 04.07.17.
  */
case class FunctionDeclaration(name: String, parameter: Seq[Type], result: Type) extends SMTLib {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write(s"declare-fun ${name} ")
    writer.write("(")
    parameter.foreach { writer.write(_).write(" ") }
    writer.write(")")
    writer.write(" ")
    writer.write(result)
  }
}
