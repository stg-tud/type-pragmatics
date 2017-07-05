package de.tu_darmstadt.veritas.backend.smtlib

import de.tu_darmstadt.veritas.backend.util.prettyprint.{PrettyPrintWriter, PrettyPrintable, SimplePrettyPrintable}

/**
  * Created by andiderp on 03.07.17.
  */
trait Term extends SMTLib

final case object True extends Term with SimplePrettyPrintable {
  override val prettyString = "true"
}

final case object False extends Term with SimplePrettyPrintable {
  override val prettyString = "false"
}

abstract class Variable(name: String) extends Term with SimplePrettyPrintable {
  override def prettyString = name
}

final case class VariableBinding(name: String, sort: Type) extends Variable(name) with SimplePrettyPrintable {
  override def prettyString: String = s"($name $sort)"
}

final case class SortedVariable(name: String, sort: Type) extends Variable(name) {
  override def prettyString = s"$name $sort"
}

final case class Let(bindings: Seq[VariableBinding], formula: Term) extends Term {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write("(let (")
    writer.write(bindings.head)
    bindings.tail.foreach { b =>
      writer.write(" ")
      writer.write(b)
    }
    writer.write(")")
    writer.write(formula)
    writer.write(")")
  }
}

// TODO: should we only allow boolean formulas in condition?
final case class IfThenElse(condition: Term, thenCase: Term, elseCase: Term) extends Term {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write("(ite ")
    writer.write(condition)
    writer.write(" ")
    writer.write(thenCase)
    writer.write(" ")
    writer.write(elseCase)
    writer.write(")")
  }
}

final case class Appl(function: String, args: Seq[Term]) extends Term {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("(")
    writer.write(function)
    writer.write(" ")
    writer.write(args.head)
    args.tail.foreach { arg =>
      writer.write(" ")
      writer.write(arg)
    }
    writer.write(")")
  }
}
