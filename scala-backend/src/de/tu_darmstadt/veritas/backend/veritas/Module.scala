package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.stratego.StrategoInt
import de.tu_darmstadt.veritas.backend.nameresolution.nabl.VeritasModuleUri
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintableFile
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

/**
 * The top-level structure of any Veritas file
 */
case class Module(name: String, imports: Seq[Import], body: Seq[ModuleDef]) extends VeritasConstruct with PrettyPrintableFile {
  override val filename = name.split('.').last + ".stl"
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("module ")
    writer.writeln(name).writeln()
    
    imports foreach (writer.writeln(_))
    
    if (!imports.isEmpty && !body.isEmpty) writer.writeln()
    
    body.dropRight(1) foreach (writer.writeln(_).writeln())
    body.lastOption foreach (writer.writeln(_))
  }
}

object Module {
  def from(term: StrategoTerm): Module = term match {
    case StrategoAppl("Module", StrategoString(name), StrategoList(imports), StrategoList(body)) => {
      Module(name, imports map Import.from, body map ModuleDef.from)
    }
    case t => throw VeritasParseError(t)
  }
}
