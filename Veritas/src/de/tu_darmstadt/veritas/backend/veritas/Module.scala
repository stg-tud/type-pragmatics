package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintableFile
import de.tu_darmstadt.veritas.backend.transformation.TransformationError

/**
 * The top-level structure of any Veritas file
 */
case class Module(name: String, imports: Seq[Import], defs: Seq[ModuleDef]) extends ModuleDefHolder with VeritasConstruct with PrettyPrintableFile {
  override val children = Seq(imports, defs)

  def fold(name: String => String, imports: Seq[Import] => Seq[Import], body: Seq[ModuleDef] => Seq[ModuleDef]) =
    Module(name(this.name), imports(this.imports), body(this.defs))
  
  override val filename = name.split('.').last + ".stl"
  override def goalname = getOnlyGoal.name
  
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("module ")
    writer.writeln(name).writeln()

    imports foreach (writer.writeln(_))

    if (!imports.isEmpty && !defs.isEmpty) writer.writeln()

    defs.dropRight(1) foreach (writer.writeln(_).writeln())
    defs.lastOption foreach (writer.writeln(_))
  }
  
  /**
   * returns the single goal at the end of the file, throws TransformationError if
   *  - there is no goal (every .fof file must have one for proof)
   *  - there is more than one (fof allows only one per file)
   *  - there are elements that are not goals after a goal (not in scope!!)
   */
  def getOnlyGoal: TypingRule = {
    defs.last match {
      case Goals(goals, /* TODO */ timeout) if (goals.length == 1) => {
        val allGoals = defs.collect { case g @ Goals(goals, _) => g }
        if (allGoals.length > 1)
          throw throw TransformationError(s"(Core TSSL) Module ${name} contained more than one goal (several positions)")
        else
          goals.head
      }
      case Goals(goals, /* TODO */ timeout) if (goals.length != 1) => throw TransformationError(s"(Core TSSL) Module ${name} contained more than one goal at last position (or illegal empty goal block)")
      case _ => throw TransformationError(s"(Core TSSL) Module ${name} contained no goal at all or did not have a goal as last construct! ${defs.last}")
    }
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
