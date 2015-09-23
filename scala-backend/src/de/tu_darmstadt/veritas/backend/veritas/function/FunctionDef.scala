package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable

case class FunctionDef(signature: FunctionSig, eqn: Seq[FunctionEq]) extends VeritasConstruct with PrettyPrintable {
  override val children = Seq(Seq(signature), eqn)

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (newchildren.length != 2 || newchildren(0).length != 1) 
      throw new ClassCastException
    try {
      val newsig: FunctionSig = newchildren(0).head match {
        case s: FunctionSig => s
        case _              => throw new ClassCastException
      }
      val neweqn: Seq[FunctionEq] = newchildren(1) map {
        case eq: FunctionEq => eq
        case _              => throw new ClassCastException
      }
      FunctionDef(newsig, neweqn)
    } catch {
      case e: NoSuchElementException => throw new ClassCastException //happens if newchildren empty!
      case e: Exception              => throw e //should not happen!
    }
  }

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(signature)
    if (!eqn.isEmpty)
      writer.writeln()
    eqn.dropRight(1) foreach (writer.writeln(_))
    eqn.lastOption foreach (writer.write(_))
  }
  
  override def toString() = s"functions \n ${signature} \n ${eqn.mkString("\n")}"
}

object FunctionDef {
  def from(term: StrategoTerm): FunctionDef = term match {
    case StrategoAppl("FunctionDef", sig, StrategoList(eqn)) => FunctionDef(FunctionSig.from(sig), eqn map FunctionEq.from)
  }
}
