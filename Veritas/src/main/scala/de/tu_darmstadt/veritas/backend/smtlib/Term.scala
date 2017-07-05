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


final case class Not private (formula: Term) extends Term {
  override def prettyPrint(writer: PrettyPrintWriter): Unit = {
    writer.write("(not ")
    writer.write(formula)
    writer.write(")")
  }
}

//object Not {
//  def apply(formula: Term): Term = formula match {
//    case True => False
//    case False => True
//    case _ => new Not(formula)
//  }
//  def unapply(e: Not): Option[Not]
//}

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
    if (args.nonEmpty) {
      writer.write("(")
      writer.write(function)
      writer.write(" ")
      writer.write(args.head)
      args.tail.foreach { arg =>
        writer.write(" ")
        writer.write(arg)
      }
      writer.write(")")
    } else // application without arguments are constructors like aempty and don't need parentheses
      writer.write(function)
  }
}
