package de.tu_darmstadt.veritas.backend.ast


import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList


sealed trait ModuleDef extends VeritasConstruct with PrettyPrintable

trait VeritasFormula extends VeritasConstruct

case class Local(defs: Seq[ModuleDef]) extends ModuleDef with ModuleDefHolder with VeritasFormula {
  // from ModuleDefHolder
  override def imports = Seq()
  
  override val children = Seq(defs)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("local { ")
    writer.indent()
    defs.dropRight(1) foreach (writer.writeln(_).writeln())
    defs.lastOption foreach (writer.writeln(_))
    writer.unindent().write("}")
  }
  
  override def toString() = s"local(${defs.mkString("\n")})"
}

case object HideAll extends ModuleDef with SimplePrettyPrintable {
  override val children = Seq()

  override def prettyString = "hide-all"
}

case class Hide(ruleNames: Seq[String]) extends ModuleDef {
  override val children = Seq()

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("hide {")
    writer.indentOptional().write(ruleNames.mkString(", "))
    writer.unindent().write("}")
  }
}

case class Include(ruleNames: Seq[String]) extends ModuleDef {
  override val children = Seq()

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("include {")
    writer.indentOptional().write(ruleNames.mkString(", "))
    writer.unindent().write("}")
  }
}

//TODO: How to deal with empty strategy, goal, lemma, axiom.... blocks? 
//Should that be allowed to occur? Currently, no errors are thrown if that happens!
//Maybe introduce require-clauses?

case class Strategy(name: String, imports: Seq[Import], defs: Seq[ModuleDef]) extends ModuleDef with ModuleDefHolder {
  override val children = Seq(imports, defs)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(s"strategy ${name} { ")
    writer.indent()

    imports foreach (writer.writeln(_))

    defs.dropRight(1) foreach (writer.writeln(_).writeln())
    defs.lastOption foreach (writer.writeln(_))
    writer.unindent().write("}")
  }
}

case class Goals(goals: Seq[TypingRule], timeout: Option[Int]) extends ModuleDef with VeritasFormula {
  override val children = Seq(goals)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    val timeoutString = timeout.map(" " + _.toString).getOrElse("")
    goals.dropRight(1) foreach (writer.writeln("goal" + timeoutString).writeln(_).writeln())
    goals.lastOption foreach (writer.writeln("goal" + timeoutString).write(_))
  }
}

case class GoalsWithStrategy(strategy: String, goals: Seq[TypingRule], timeout: Option[Int]) extends ModuleDef with VeritasFormula {
  override val children = Seq(goals)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    val timeoutString = timeout.map(" " + _.toString).getOrElse("")
    goals.dropRight(1) foreach (writer.writeln(s"goal verify-with ${strategy} " + timeoutString).writeln(_).writeln())
    goals.lastOption foreach (writer.writeln(s"goal verify-with ${strategy} " + timeoutString).write(_))
  }
}

case class Lemmas(lemmas: Seq[TypingRule], timeout: Option[Int]) extends ModuleDef with VeritasFormula {
  override val children = Seq(lemmas)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    val timeoutString = timeout.map(" " + _.toString).getOrElse("")
    lemmas.dropRight(1) foreach (writer.writeln("lemma" + timeoutString).writeln(_).writeln())
    lemmas.lastOption foreach (writer.writeln("lemma" + timeoutString).write(_))
  }
}

case class LemmasWithStrategy(strategy: String, lemmas: Seq[TypingRule], timeout: Option[Int]) extends ModuleDef with VeritasFormula {
  override val children = Seq(lemmas)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    val timeoutString = timeout.map(" " + _.toString).getOrElse("")
    lemmas.dropRight(1) foreach (writer.writeln(s"lemma verify-with ${strategy} " + timeoutString).writeln(_).writeln())
    lemmas.lastOption foreach (writer.writeln(s"lemma verify-with ${strategy} " + timeoutString).write(_))
  }
}

case class Axioms(axioms: Seq[TypingRule]) extends ModuleDef with VeritasFormula {
  override val children = Seq(axioms)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    axioms.dropRight(1) foreach (writer.writeln("axiom").writeln(_).writeln())
    axioms.lastOption foreach (writer.writeln("axiom").write(_))
  }
}

case class Sorts(sorts: Seq[SortDef]) extends ModuleDef {
  override val children = Seq(sorts)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("sorts  ")
    writer.indentOptional()
    sorts foreach (writer.write(_).write(" "))
    writer.unindent()
  }
}


case class Functions(funcs: Seq[FunctionDef]) extends ModuleDef {
  override val children = Seq(funcs)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    funcs.dropRight(1) foreach (writer.writeln("function").writeln(_).writeln())
    funcs.lastOption foreach (writer.writeln("function").write(_))
  }
}

case class PartialFunctions(funcs: Seq[FunctionDef]) extends ModuleDef {
  override val children = Seq(funcs)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    funcs.dropRight(1) foreach (writer.writeln("partial function").writeln(_))
    funcs.lastOption foreach (writer.writeln("partial function").write(_))
  }
}

case class Consts(consts: Seq[ConstDecl], different: Boolean) extends ModuleDef {
  override val children = Seq(consts)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    if (different)
      writer.write("different ")
    writer.write("consts ")
    writer.indent()
    consts foreach { c =>
      c.prettyPrint(writer)
      writer.writeln()
    }
    writer.unindent()
  }
}

object Consts {
  def Different(term: StrategoTerm) = term match {
    case StrategoAppl("Any") => false
    case StrategoAppl("Different") => true
    case t => throw VeritasParseError(t)
  }
}

case class DataType(open: Boolean, name: String, constrs: Seq[DataTypeConstructor]) extends ModuleDef {
  require(!(DataType.predefinedTypes contains name), s"Cannot redefine predefined type $name")

  override val children = Seq(constrs)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    if (open)
      writer.write("open ")
    writer.write("data ")
    writer.write(name)
    if (!constrs.isEmpty) {
      writer.write(" = ")
      writer.indent()
      constrs(0).prettyPrint(writer)
      constrs.tail foreach {c =>
        writer.writeln(" |")
        c.prettyPrint(writer)
      }
      writer.unindent()
    }
  }

  override def toString() = {
    val dtstring = s"data $name = ${constrs.mkString(" | ")}"
    if (open)
      "open " + dtstring
    else
      dtstring
  }
}

object DataType {
  val Bool = "Bool"
  val iType = "iType"
  val predefinedTypes: List[String] = List(Bool, iType)

  def Openedness(term: StrategoTerm): Boolean = term match {
    case StrategoAppl("Sealed") => false
    case StrategoAppl("Open") => true
    case t => throw VeritasParseError(t)
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
    case StrategoAppl("HideAll")                   => HideAll
    case StrategoAppl("Hide", StrategoList(ruleNames)) => Hide(ruleNames map {
      case StrategoString(name) => name
      case t                    => throw VeritasParseError("expected StrategoString, got: " + t)
    })
    case StrategoAppl("Include", StrategoList(ruleNames)) => Include(ruleNames map {
      case StrategoString(name) => name
      case t                    => throw VeritasParseError("expected StrategoString, got: " + t)
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
    case StrategoAppl("Functions", StrategoList(funcDefs)) => Functions(funcDefs map FunctionDef.from)
    case StrategoAppl("PartialFunctions", StrategoList(funcDefs)) => PartialFunctions(funcDefs map FunctionDef.from)
    case StrategoAppl("Consts", different, StrategoList(consts)) => Consts(consts map ConstDecl.from, Consts.Different(different))
    case StrategoAppl("DataType", openedness, StrategoString(name), StrategoList(constrs)) => DataType(DataType.Openedness(openedness), name, constrs map DataTypeConstructor.from)
    case t => throw VeritasParseError(t)
  }
}
