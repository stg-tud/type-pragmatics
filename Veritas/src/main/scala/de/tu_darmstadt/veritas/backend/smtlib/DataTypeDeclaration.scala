package de.tu_darmstadt.veritas.backend.smtlib

import de.tu_darmstadt.veritas.backend.util.prettyprint.{PrettyPrintWriter, SimplePrettyPrintable}

/**
  * Created by andiderp on 03.07.17.
  */
case class DataTypeDeclaration(name: String, cotrs: Seq[Constructor]) extends SMTLib {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write("(declare-datatypes ")
    writer.write(s"(($name 0))")
    writer.write("((")
    cotrs.foreach { writer.write(_).write(" ") }
    writer.write(")))")
  }
}

case class Sort(name: String) extends SMTLib {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write(s"(declare-sort ${name} 0)")
  }
}

case class Constructor(name: String, selectors: Seq[Selector]) extends SMTLib {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write(s"$name")
    if (selectors.nonEmpty)
      writer.write(" ")
    selectors.foreach { sel =>
      writer.write(sel)
      writer.write(" ")
    }
  }
}

case class Selector(name: String, returnType: Type) extends SMTLib {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write(name)
    writer.write(" ")
    writer.write(returnType)
  }
}

case class Type(name: String) extends SimplePrettyPrintable {
  override def prettyString: String = name
}
