package de.tu_darmstadt.veritas.backend.smtlib

import de.tu_darmstadt.veritas.backend.util.prettyprint.{PrettyPrintWriter, PrettyPrintable, SimplePrettyPrintable}

/**
  * Created by andiderp on 03.07.17.
  */
sealed trait Declaration extends SMTLib

case class DataTypeDeclaration(name: String, cotrs: Seq[Constructor]) extends Declaration {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write("(declare-datatypes ")
    writer.write(s"(($name 0)) ")
    writer.write("( (")
    writer.write(cotrs.head)
    cotrs.tail.foreach { cotr =>
      writer.write(" ")
      writer.write(cotr)
    }
    writer.write(") ) )")
  }
}

case class Sort(name: String) extends Declaration {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write(s"(declare-sort ${name} 0)")
  }
}

case class Constructor(name: String, selectors: Seq[Selector]) extends PrettyPrintable {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write(s"($name")
    if (selectors.nonEmpty)
      selectors.foreach { sel =>
        writer.write(" ")
        writer.write(sel)
      }
    writer.write(")")
  }
}

case class Selector(name: String, returnType: Type) extends PrettyPrintable {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write(s"($name")
    writer.write(" ")
    writer.write(returnType)
    writer.write(")")
  }
}

case class Type(name: String) extends SimplePrettyPrintable {
  override def prettyString: String = name
}

case class FunctionDeclaration(name: String, parameter: Seq[Type], result: Type) extends Declaration {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write(s"(declare-fun ${name} ")
    writer.write("(")
    writer.write(parameter.head)
    parameter.tail.foreach { p =>
      writer.write(" ")
      writer.write(p)
    }
    writer.write(")")
    writer.write(" ")
    writer.write(result)
    writer.write(")")
  }
}
