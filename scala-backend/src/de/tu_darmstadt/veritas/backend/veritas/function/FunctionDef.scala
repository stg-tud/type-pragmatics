package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable

case class FunctionDef(signature: FunctionSig, eqn: Seq[FunctionEq]) extends PrettyPrintable {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(signature)
    if (!eqn.isEmpty) 
      writer.writeln()
    eqn.dropRight(1) foreach (writer.writeln(_))
    eqn.lastOption foreach (writer.write(_))
  }
}

object FunctionDef {
  def from(term: StrategoTerm): FunctionDef = term match {
    case StrategoAppl("FunctionDef", sig, StrategoList(eqn)) 
      => FunctionDef(FunctionSig.from(sig), eqn map FunctionEq.from)
  }
}
