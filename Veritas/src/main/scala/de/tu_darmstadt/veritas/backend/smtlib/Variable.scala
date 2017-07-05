package de.tu_darmstadt.veritas.backend.smtlib

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

/**
  * Created by andiderp on 06.07.17.
  */
abstract class Variable(name: String) extends Term {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write(name)
  }
}

final case class VariableBinding(name: String, formula: Term) extends Variable(name) {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write(s"($name ")
    writer.write(formula)
    writer.write(")")
  }
}

final case class VariableReference(name: String) extends Variable(name)

final case class SortedVariable(name: String, sort: Type) extends Variable(name) {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write(s"$name ")
    writer.write(sort)
  }
}
