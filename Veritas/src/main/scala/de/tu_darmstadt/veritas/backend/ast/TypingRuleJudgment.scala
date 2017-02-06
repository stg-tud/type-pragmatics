package de.tu_darmstadt.veritas.backend.ast

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.ast.function._

sealed trait TypingRuleJudgment extends VeritasConstruct with PrettyPrintable

case class TypingJudgment(f1: FunctionExpMeta, f2: FunctionExpMeta, f3: FunctionExpMeta) extends TypingRuleJudgment {
  override val children = Seq(Seq(f1), Seq(f2), Seq(f3))

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(f1).write(" |- ")
    writer.write(f2).write(" : ")
    writer.write(f3)
  }
  
  override def toString() = s"${f1} |- ${f2} : ${f3}"
}

case class TypingJudgmentSimple(f1: FunctionExpMeta, f2: FunctionExpMeta) extends TypingRuleJudgment {
  override val children = Seq(Seq(f1), Seq(f2))

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(f1).write(" : ")
    writer.write(f2)
  }
  
  override def toString() = s"${f1} : ${f2}"
}

case class FunctionExpJudgment(f: FunctionExp) extends TypingRuleJudgment {
  override val children = Seq(Seq(f))

  override def prettyPrint(writer: PrettyPrintWriter) = writer.write(f)
  override def toString() = f.toString()
}

/**
 * convenience, such that MetaVars don't have to be explicitly wrapped when used inside as FunctionExp
 */
object FunctionExpJudgment {
  implicit def wrap(f: FunctionExp): FunctionExpJudgment = FunctionExpJudgment(f)
}

case class ExistsJudgment(varlist: Seq[MetaVar], jdglst: Seq[TypingRuleJudgment]) extends TypingRuleJudgment {
  override val children = Seq(varlist, jdglst)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("exists ")
    varlist.dropRight(1) foreach (writer.write(_).write(", "))
    varlist.lastOption foreach (writer.write(_))
    writer.indent()
    jdglst.dropRight(1) foreach (writer.writeln(_))
    jdglst.lastOption foreach (writer.write(_))
    writer.unindent()
  }
  
  override def toString() = s"exists ${varlist.mkString(",")}: \n ${jdglst.mkString("=> " , "\n", "")}"
}

case class ForallJudgment(varlist: Seq[MetaVar], jdglst: Seq[TypingRuleJudgment]) extends TypingRuleJudgment {
  override val children = Seq(varlist, jdglst)

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("forall ")
    varlist.dropRight(1) foreach (writer.write(_).write(", "))
    varlist.lastOption foreach (writer.write(_))
    writer.indent()
    jdglst.dropRight(1) foreach (writer.writeln(_))
    jdglst.lastOption foreach (writer.write(_))
    writer.unindent()
  }
  
  override def toString() = s"forall ${varlist.mkString(",")}: \n ${jdglst.mkString("=> " , "\n", "")}"
}

// TODO untested, no example in Veritas/test/*.stl files
// TODO decide whether we keep this or not
case class ReduceJudgment(f1: FunctionExpMeta, f2: FunctionExpMeta) extends TypingRuleJudgment {
  override val children = Seq(Seq(f1), Seq(f2))

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write(f1).write(" -> ")
    writer.write(f2)
  }
  
  override def toString() = s"${f1} -> ${f2}"
}

// TODO untested, no example in Veritas/test/*.stl files
case class NotJudgment(jdg: TypingRuleJudgment) extends TypingRuleJudgment {
  override val children = Seq(Seq(jdg))

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("not ")
    writer.write(jdg)
  }
  
  override def toString() = "not(" + jdg.toString + ")"
}

case class OrJudgment(orCases: Seq[Seq[TypingRuleJudgment]]) extends TypingRuleJudgment {
  override val children = orCases

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.writeln("OR")
    orCases.dropRight(1) foreach { orCase =>
      writer.write("=> ")
      orCase foreach (writer.writeln(_))
    }
    orCases.lastOption foreach { orCase =>
      writer.write("=> ")
      orCase.dropRight(1) foreach (writer.writeln(_))
      orCase.lastOption foreach (writer.write(_))
    }
  }
  
  override def toString() = s"Or \n ${orCases.mkString("(=> " , "\n => ", " )")}"
}

object TypingRuleJudgment {
  def from(term: StrategoTerm): TypingRuleJudgment = term match {
    case StrategoAppl("TypingJudgment", f1, f2, f3)   => TypingJudgment(FunctionExpMeta.from(f1), FunctionExpMeta.from(f2), FunctionExpMeta.from(f3))
    case StrategoAppl("TypingJudgmentSimple", f1, f2) => TypingJudgmentSimple(FunctionExpMeta.from(f1), FunctionExpMeta.from(f2))
    case StrategoAppl("FunctionExpJudgment", f)       => FunctionExpJudgment(FunctionExp.from(f))
    case StrategoAppl("ExistsJudgment", StrategoList(metavars), jdglst) => ExistsJudgment(
      metavars map MetaVar.from,
      TypingRule.unpackJudgmentCons(jdglst) map TypingRuleJudgment.from)
    case StrategoAppl("ForallJudgment", StrategoList(metavars), jdglst) => ForallJudgment(
      metavars map MetaVar.from,
      TypingRule.unpackJudgmentCons(jdglst) map TypingRuleJudgment.from)
    case StrategoAppl("OrJudgement", orCases) => OrJudgment(
      unpackOrCaseList(orCases) map { jdglst =>
        TypingRule.unpackJudgmentCons(jdglst) map TypingRuleJudgment.from
      })
    case StrategoAppl("ReduceJudgment", f1, f2) => ReduceJudgment(FunctionExpMeta.from(f1), FunctionExpMeta.from(f2))
    case StrategoAppl("NotJudgment", jdg)       => NotJudgment(TypingRuleJudgment.from(jdg))
    case t                                      => throw VeritasParseError(t)
  }

  def unpackOrCaseList(term: StrategoTerm): Seq[StrategoTerm] = term match {
    case StrategoAppl("OrCons", head, rest) => head +: unpackOrCaseList(rest)
    case StrategoAppl("OrEnd", elem)        => Seq(elem)
    case t                                  => throw VeritasParseError("expected Or cases (OrCons/OrEnd), got: " + t)
  }
}
