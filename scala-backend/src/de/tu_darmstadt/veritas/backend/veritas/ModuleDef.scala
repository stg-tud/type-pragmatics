package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.stratego.StrategoInt
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.util.Context
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable

sealed trait ModuleDef extends PrettyPrintable

case class Local(defs: Seq[ModuleDef]) extends ModuleDef {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("local { ")
    writer.indent()
    defs.dropRight(1) foreach (writer.writeln(_).writeln())
    defs.lastOption foreach (writer.writeln(_))
    writer.unindent().write("}")
  }
}

case object HideAll extends ModuleDef with SimplePrettyPrintable {
  override def prettyString = "hide-all"
}

case class Hide(ruleNames: Seq[String]) extends ModuleDef {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("hide {")
    writer.indentOptional().write(ruleNames.mkString(", "))
    writer.unindent().write("}")
  }
}

case class Include(ruleNames: Seq[String]) extends ModuleDef {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("include {")
    writer.indentOptional().write(ruleNames.mkString(", "))
    writer.unindent().write("}")
  }
}

case class Strategy(name: String, imports: Seq[Import], defs: Seq[ModuleDef]) extends ModuleDef {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(s"strategy ${name} { ")
    writer.indent()
    
    imports foreach (writer.writeln(_))
    
    defs.dropRight(1) foreach (writer.writeln(_).writeln())
    defs.lastOption foreach (writer.writeln(_))
    writer.unindent().write("}")
  }
}

case class Goals(goals: Seq[TypingRule], timeout: Option[Int]) extends ModuleDef {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    val timeoutString = timeout.map(" " + _.toString).getOrElse("")
    goals.dropRight(1) foreach (writer.writeln("goal" + timeoutString).writeln(_).writeln())
    goals.lastOption foreach (writer.writeln("goal"+ timeoutString).write(_))
  }
}

case class GoalsWithStrategy(strategy: String, goals: Seq[TypingRule], timeout: Option[Int]) extends ModuleDef {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    val timeoutString = timeout.map(" " + _.toString).getOrElse("")
    goals.dropRight(1) foreach (writer.writeln(s"goal verify-with ${strategy} " + timeoutString).writeln(_).writeln())
    goals.lastOption foreach (writer.writeln(s"goal verify-with ${strategy} " + timeoutString).write(_))
  }
}

case class Lemmas(lemmas: Seq[TypingRule], timeout: Option[Int]) extends ModuleDef {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    val timeoutString = timeout.map(" " + _.toString).getOrElse("")
    lemmas.dropRight(1) foreach (writer.writeln("lemma" + timeoutString).writeln(_).writeln())
    lemmas.lastOption foreach (writer.writeln("lemma"+ timeoutString).write(_))
  }
}

case class LemmasWithStrategy(strategy: String, lemmas: Seq[TypingRule], timeout: Option[Int]) extends ModuleDef {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    val timeoutString = timeout.map(" " + _.toString).getOrElse("")
    lemmas.dropRight(1) foreach (writer.writeln(s"lemma verify-with ${strategy} " + timeoutString).writeln(_).writeln())
    lemmas.lastOption foreach (writer.writeln(s"lemma verify-with ${strategy} " + timeoutString).write(_))
  }
}

case class Axioms(axioms: Seq[TypingRule]) extends ModuleDef {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    axioms.dropRight(1) foreach (writer.writeln("axiom").writeln(_).writeln())
    axioms.lastOption foreach (writer.writeln("axiom").write(_))
  }
}

case class Sorts(sorts: Seq[SortDef]) extends ModuleDef {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("sorts  ")
    writer.indentOptional()
    sorts foreach (writer.write(_).write(" "))
    writer.unindent()
  }
}

case class Constructors(ctors: Seq[ConstructorDecl]) extends ModuleDef {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("constructors")
    writer.indent()
    ctors.dropRight(1) foreach (writer.writeln(_))
    ctors.lastOption foreach (writer.write(_))
    writer.unindent()
  }
}

case class Functions(funcs: Seq[FunctionDef]) extends ModuleDef {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    funcs.dropRight(1) foreach (writer.writeln("function").writeln(_).writeln())
    funcs.lastOption foreach (writer.writeln("function").write(_))
  }
}

case class PartialFunctions(funcs: Seq[FunctionDef]) extends ModuleDef {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    funcs.dropRight(1) foreach (writer.writeln("partial function").writeln(_))
    funcs.lastOption foreach (writer.writeln("partial function").write(_))
  }
}

case class Consts(consts: Seq[ConstructorDecl]) extends ModuleDef {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    consts.dropRight(1) foreach { c => 
      writer.write("const ");
      writer.writeln(c)
    }
    consts.lastOption foreach { c => 
      writer.write("const ");
      writer.write(c)
    }
  }
}

// TODO add relation, "single" function (?), functions, const, sort (?), ctor (?) etc.
// FIXME RelationDef vs FunctionDef? Relations vs PartialFunctions?
//case class PartialFunctions(partialFunctions: Seq[RelationDef]) extends ModuleDef
//case class Consts(consts: Seq[ConstDecl]) extends ModuleDef
//case class RelationDef()
//case class ConstDecl()

object ModuleDef {
  def from(term: StrategoTerm): ModuleDef = term match {
    case StrategoAppl("Local", StrategoList(defs)) => Local(defs map ModuleDef.from)
    case StrategoAppl("HideAll") => HideAll
    case StrategoAppl("Hide", StrategoList(ruleNames)) => Hide(ruleNames map {
      case StrategoString(name) => name
      case t => throw VeritasParseError("expected StrategoString, got: " + t)
    })
    case StrategoAppl("Include", StrategoList(ruleNames)) => Include(ruleNames map {
      case StrategoString(name) => name
      case t => throw VeritasParseError("expected StrategoString, got: " + t)
    })
    case StrategoAppl("Strategy", StrategoString(name), StrategoList(imps), StrategoList(defs)) => Strategy(name, imps map Import.from, defs map ModuleDef.from)
    case StrategoAppl("Goals", StrategoAppl("None"), StrategoList(goals)) => Goals(goals map TypingRule.from, None)
    case StrategoAppl("Goals", StrategoAppl("Some", StrategoString(timeout)), StrategoList(goals)) => Goals(goals map TypingRule.from, Some(timeout.toInt))
    case StrategoAppl("GoalsWithStrategy", StrategoString(strategy), StrategoAppl("None"), StrategoList(goals)) => GoalsWithStrategy(strategy, goals map TypingRule.from, None)
    case StrategoAppl("GoalsWithStrategy", StrategoString(strategy), StrategoAppl("Some", StrategoString(timeout)), StrategoList(goals)) => GoalsWithStrategy(strategy, goals map TypingRule.from, Some(timeout.toInt))
    case StrategoAppl("Lemmas", StrategoAppl("None"), StrategoList(lemmas)) => Lemmas(lemmas map TypingRule.from, None)
    case StrategoAppl("Lemmas", StrategoAppl("Some", StrategoString(timeout)), StrategoList(lemmas)) => Lemmas(lemmas map TypingRule.from, Some(timeout.toInt))
    case StrategoAppl("LemmasWithStrategy", StrategoString(strategy), StrategoAppl("None"), StrategoList(lemmas)) => LemmasWithStrategy(strategy, lemmas map TypingRule.from, None)
    case StrategoAppl("LemmasWithStrategy", StrategoString(strategy), StrategoAppl("Some", StrategoString(timeout)), StrategoList(lemmas)) => LemmasWithStrategy(strategy, lemmas map TypingRule.from, Some(timeout.toInt))
    case StrategoAppl("Axioms", StrategoList(axioms)) => Axioms(axioms map TypingRule.from)
    case StrategoAppl("Sorts", StrategoList(sorts)) => Sorts(sorts map SortDef.from)
    case StrategoAppl("Constructors", StrategoList(ctors)) => Constructors(ctors map ConstructorDecl.from)
    case StrategoAppl("Functions", StrategoList(funcDefs)) => Functions(funcDefs map FunctionDef.from)
    case StrategoAppl("PartialFunctions", StrategoList(funcDefs)) => PartialFunctions(funcDefs map FunctionDef.from)
    case StrategoAppl("Consts", StrategoList(consts)) => Consts(consts map ConstructorDecl.from)
    case t => throw VeritasParseError(t)
  }
}
