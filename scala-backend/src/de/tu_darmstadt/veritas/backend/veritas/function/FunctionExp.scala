package de.tu_darmstadt.veritas.backend.veritas

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable
import de.tu_darmstadt.veritas.backend.stratego.StrategoList

sealed trait FunctionExp extends PrettyPrintable

final case class FunctionExpNot(f: FunctionExp) extends FunctionExp {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("!")
    writer.write(f)
  }
}

final case class FunctionExpEq(f1: FunctionExp, f2: FunctionExp) extends FunctionExp {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    // NOTE according to Veritas.sdf3 both pretty printings are correct: " = " and " == "
    writer.write("(")
    writer.write(f1).write(" == ")
    writer.write(f2).write(")")
  }
}

final case class FunctionExpNeq(f1: FunctionExp, f2: FunctionExp) extends FunctionExp {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("(")
    writer.write(f1).write(" != ")
    writer.write(f2).write(")")
  }
}

final case class FunctionExpAnd(left: FunctionExp, right: FunctionExp) extends FunctionExp {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("(")
    writer.write(left).write(" && ")
    writer.write(right).write(")")
  }
}

/**
 * convenience to create more than binary ands (and also optimization if args.isEmpty => gives just true)
 */
object FunctionExpAnd {
  def apply(args: Seq[FunctionExp]): FunctionExp = args match {
    case Seq() => FunctionExpTrue
    case Seq(oneArg) => oneArg
    case Seq(head,rest@_*) => FunctionExpAnd(head, FunctionExpAnd(rest))
  }
}

final case class FunctionExpOr(left: FunctionExp, right: FunctionExp) extends FunctionExp {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("(")
    writer.write(left).write(" || ")
    writer.write(right).write(")")
  }
}

final case class FunctionExpBiImpl(left: FunctionExp, right: FunctionExp) extends FunctionExp {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("(")
    writer.write(left).write(" <=> ")
    writer.write(right).write(")")
  }
}

final case class FunctionExpIf(cond: FunctionExp, thenE: FunctionExp, elseE: FunctionExp) extends FunctionExp {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("if ")
    writer.writeln(cond)
    writer.write("then ")
    writer.writeln(thenE)
    writer.write("else ")
    writer.write(elseE)
  }
}

final case class FunctionExpLet(name: String, namedExpr: FunctionExp, in: FunctionExp) extends FunctionExp {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("let ", name, " = ").write(namedExpr).write(" in ")
    writer.indentOptional().write(in).unindent()
  }
}

final case class FunctionExpApp(functionName: String, args: Seq[FunctionExp]) extends FunctionExp {
  override def prettyPrint(writer: PrettyPrintWriter) = {
    // NOTE the parenthesis are necessary! They distinguish an empty-args function application (e.g.
    // "something()" from a variable, bound by the pattern on the left side of a FunctionEq (just
    // "something")
    writer.write(functionName, "(")
    args.dropRight(1) foreach (writer.write(_).write(", "))
    args.lastOption foreach (writer.write(_))
    writer.write(")")
  }
}

final case class FunctionExpMeta(metavar: MetaVar) extends FunctionExp with SimplePrettyPrintable {
  override def prettyString = metavar.toPrettyString
}

/**
 * convenience, such that MetaVars don't have to be explicitly wrapped when used inside as FunctionExp
 */
object FunctionExpMeta {
  implicit def wrap(metavar: MetaVar): FunctionExpMeta = FunctionExpMeta(metavar)
}

final case class FunctionExpVar(name: String) extends FunctionExp with SimplePrettyPrintable {
  override def prettyString = name
}

final case object FunctionExpTrue extends FunctionExp with SimplePrettyPrintable {
  override val prettyString = "true"
}

final case object FunctionExpFalse extends FunctionExp with SimplePrettyPrintable {
  override val prettyString = "false"
}

object FunctionExp {
  def from(term: StrategoTerm): FunctionExp = term match {
    case StrategoAppl("FunctionExpNot", f)
      => FunctionExpNot(FunctionExp.from(f))
    case StrategoAppl("FunctionExpEq", f1, f2)
      => FunctionExpEq(FunctionExp.from(f1), FunctionExp.from(f2))
    case StrategoAppl("FunctionExpNeq", f1, f2)
      => FunctionExpNeq(FunctionExp.from(f1), FunctionExp.from(f2))
    case StrategoAppl("FunctionExpAnd", f1, f2)
      => FunctionExpAnd(FunctionExp.from(f1), FunctionExp.from(f2))
    case StrategoAppl("FunctionExpOr", f1, f2)
      => FunctionExpOr(FunctionExp.from(f1), FunctionExp.from(f2))
    case StrategoAppl("FunctionExpBiImpl", f1, f2)
      => FunctionExpBiImpl(FunctionExp.from(f1), FunctionExp.from(f2))
    case StrategoAppl("FunctionExpIf", cond, f1, f2)
      => FunctionExpIf(FunctionExp.from(cond), FunctionExp.from(f1), FunctionExp.from(f2))
    case StrategoAppl("FunctionExpLet", StrategoString(name), namedExpr, in)
      => FunctionExpLet(name, FunctionExp.from(namedExpr), FunctionExp.from(in))
    case StrategoAppl("FunctionExpApp", StrategoString(func), StrategoList(args)) 
      => FunctionExpApp(func, args map FunctionExp.from)
    case StrategoAppl("FunctionExpMeta", metavar) => FunctionExpMeta(MetaVar.from(metavar))
    case StrategoAppl("FunctionExpVar", StrategoString(name)) => FunctionExpVar(name)
    case StrategoAppl("FunctionExpTrue") => FunctionExpTrue
    case StrategoAppl("FunctionExpFalse") => FunctionExpFalse
    case t => throw VeritasParseError(t)
  }
}