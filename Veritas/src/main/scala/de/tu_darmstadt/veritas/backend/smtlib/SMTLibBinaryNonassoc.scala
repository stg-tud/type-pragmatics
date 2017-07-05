package de.tu_darmstadt.veritas.backend.smtlib

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

/**
  * Created by andiderp on 03.07.17.
  */
sealed abstract class SMTLibBinaryNonassoc(argLeft:  Term,
                                        argRight: Term,
                                        opString: String) extends Term {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(s"($opString ")
    writer.write(argLeft).write(" ")
    writer.write(argRight)
    writer.write(")")
  }
}

final class Impl private(val argLeft: Term, val argRight: Term)
  extends SMTLibBinaryNonassoc(argLeft, argRight, "=>") {
  override def toString = s"Impl($argLeft, $argRight)"
}
object Impl {
  def apply(argLeft: Term, argRight: Term) = new Impl(argLeft, argRight)

  def unapply(e: Impl): Option[(Term, Term)] = Some((e.argLeft, e.argRight))
}
