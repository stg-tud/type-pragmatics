package de.tu_darmstadt.veritas.backend.smtlib

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

/**
  * Created by andiderp on 03.07.17.
  */
sealed abstract class SMTLibBinaryAssoc(args: Seq[Term], opString: String) extends Term {
  require(args.size >= 2, "And/Or should never be created with less than 2 arguments, see the companion objects")

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(s"($opString ")
    writer.write(args.head)
    args.tail.foreach { su =>
      writer.write(" ")
      writer.write(su)
    }
    writer.write(")")
  }
}

final class Or private (val args: Seq[Term]) extends SMTLibBinaryAssoc(args, "or") {
  override def toString = args.mkString("Or(", ", ", ")")
}
object Or {
  def apply(args: Seq[Term]): SMTLib =
    args match {
      case Seq()       => False
      case Seq(single) => single
      case s           => new Or(s)
    }
  def unapply(e: Or): Option[Seq[Term]] = Some(e.args)
}

final class And private (val args: Seq[Term]) extends SMTLibBinaryAssoc(args, "and") {
  override def toString = args.mkString("And(", ", ", ")")
}
object And {
  def apply(args: Seq[Term]): SMTLib =
    args match {
      case Seq()       => True
      case Seq(single) => single
      case s           => new And(s)
    }

  def unapply(e: And): Option[Seq[Term]] = Some(e.args)
}
