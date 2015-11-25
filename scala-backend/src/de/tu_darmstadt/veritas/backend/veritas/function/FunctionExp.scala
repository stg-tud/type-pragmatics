package de.tu_darmstadt.veritas.backend.veritas.function

import de.tu_darmstadt.veritas.backend.stratego.StrategoAppl
import de.tu_darmstadt.veritas.backend.stratego.StrategoString
import de.tu_darmstadt.veritas.backend.stratego.StrategoTerm
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable
import de.tu_darmstadt.veritas.backend.stratego.StrategoList
import de.tu_darmstadt.veritas.backend.veritas._

sealed trait FunctionExpMeta extends VeritasConstruct with PrettyPrintable

sealed trait FunctionExp extends FunctionExpMeta with PrettyPrintable

final case class FunctionExpNot(f: FunctionExp) extends FunctionExp {
  override val children = Seq(Seq(f))

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (newchildren.length != 1 || !(newchildren forall (_.length == 1)))
      throw new ClassCastException

    val newexp: FunctionExp = newchildren(0).head match {
      case e: FunctionExp => e
      case _              => throw new ClassCastException
    }
    FunctionExpNot(newexp)
  }

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("!")
    writer.write(f)
  }

  override def toString() = s"!(${f})"
}

final case class FunctionExpEq(f1: FunctionExpMeta, f2: FunctionExpMeta) extends FunctionExp {
  override val children = Seq(Seq(f1), Seq(f2))

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (newchildren.length != 2 || !(newchildren forall (_.length == 1)))
      throw new ClassCastException

    val newexp1: FunctionExpMeta = newchildren(0).head match {
      case e: FunctionExpMeta => e
      case _                  => throw new ClassCastException
    }
    val newexp2: FunctionExpMeta = newchildren(1).head match {
      case e: FunctionExpMeta => e
      case _                  => throw new ClassCastException
    }
    FunctionExpEq(newexp1, newexp2)
  }

  override def prettyPrint(writer: PrettyPrintWriter) = {
    // NOTE according to Veritas.sdf3 both pretty printings are correct: " = " and " == "
    writer.write("(")
    writer.write(f1).write(" == ")
    writer.write(f2).write(")")
  }

  override def toString() = s"(${f1}) == (${f2})"
}

final case class FunctionExpNeq(f1: FunctionExpMeta, f2: FunctionExpMeta) extends FunctionExp {
  override val children = Seq(Seq(f1), Seq(f2))

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (newchildren.length != 2 || !(newchildren forall (_.length == 1)))
      throw new ClassCastException

    val newexp1: FunctionExpMeta = newchildren(0).head match {
      case e: FunctionExpMeta => e
      case _                  => throw new ClassCastException
    }
    val newexp2: FunctionExpMeta = newchildren(1).head match {
      case e: FunctionExpMeta => e
      case _                  => throw new ClassCastException
    }
    FunctionExpNeq(newexp1, newexp2)
  }

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("(")
    writer.write(f1).write(" != ")
    writer.write(f2).write(")")
  }

  override def toString() = s"(${f1}) != (${f2})"
}

final case class FunctionExpAnd(left: FunctionExp, right: FunctionExp) extends FunctionExp {
  override val children = Seq(Seq(left), Seq(right))

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (newchildren.length != 2 || !(newchildren forall (_.length == 1)))
      throw new ClassCastException

    val newexp1: FunctionExp = newchildren(0).head match {
      case e: FunctionExp => e
      case _              => throw new ClassCastException
    }
    val newexp2: FunctionExp = newchildren(1).head match {
      case e: FunctionExp => e
      case _              => throw new ClassCastException
    }
    FunctionExpAnd(newexp1, newexp2)
  }

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("(")
    writer.write(left).write(" && ")
    writer.write(right).write(")")
  }

  override def toString() = s"(${left}) && (${right})"
}

/**
 * convenience to create more than binary ands (and also optimization if args.isEmpty => gives just true)
 */
object FunctionExpAnd {
  def apply(args: Seq[FunctionExp]): FunctionExp = args match {
    case Seq()                => FunctionExpTrue
    case Seq(oneArg)          => oneArg
    case Seq(head, rest @ _*) => FunctionExpAnd(head, FunctionExpAnd(rest))
  }
}

final case class FunctionExpOr(left: FunctionExp, right: FunctionExp) extends FunctionExp {
  override val children = Seq(Seq(left), Seq(right))

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (newchildren.length != 2 || !(newchildren forall (_.length == 1)))
      throw new ClassCastException

    val newexp1: FunctionExp = newchildren(0).head match {
      case e: FunctionExp => e
      case _              => throw new ClassCastException
    }
    val newexp2: FunctionExp = newchildren(1).head match {
      case e: FunctionExp => e
      case _              => throw new ClassCastException
    }
    FunctionExpOr(newexp1, newexp2)
  }

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("(")
    writer.write(left).write(" || ")
    writer.write(right).write(")")
  }

  override def toString() = s"(${left}) || (${right})"
}

final case class FunctionExpBiImpl(left: FunctionExp, right: FunctionExp) extends FunctionExp {
  override val children = Seq(Seq(left), Seq(right))

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (newchildren.length != 2 || !(newchildren forall (_.length == 1)))
      throw new ClassCastException

    val newexp1: FunctionExp = newchildren(0).head match {
      case e: FunctionExp => e
      case _              => throw new ClassCastException
    }
    val newexp2: FunctionExp = newchildren(1).head match {
      case e: FunctionExp => e
      case _              => throw new ClassCastException
    }
    FunctionExpBiImpl(newexp1, newexp2)
  }

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("(")
    writer.write(left).write(" <=> ")
    writer.write(right).write(")")
  }

  override def toString() = s"(${left}) <=> (${right})"
}

final case class FunctionExpIf(cond: FunctionExpMeta, thenE: FunctionExpMeta, elseE: FunctionExpMeta) extends FunctionExp {
  override val children = Seq(Seq(cond), Seq(thenE), Seq(elseE))

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (newchildren.length != 3 || !(newchildren forall (_.length == 1)))
      throw new ClassCastException

    val newcond: FunctionExpMeta = newchildren(0).head match {
      case e: FunctionExpMeta => e
      case _                  => throw new ClassCastException
    }
    val newthen: FunctionExpMeta = newchildren(1).head match {
      case e: FunctionExpMeta => e
      case _                  => throw new ClassCastException
    }
    val newelse: FunctionExpMeta = newchildren(2).head match {
      case e: FunctionExpMeta => e
      case _                  => throw new ClassCastException
    }
    FunctionExpIf(newcond, newthen, newelse)
  }

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("if ")
    writer.writeln(cond)
    writer.write("then ")
    writer.writeln(thenE)
    writer.write("else ")
    writer.write(elseE)
  }

  override def toString() = s"if (${cond}) then (${thenE}) else (${elseE})"
}

final case class FunctionExpLet(name: String, namedExpr: FunctionExpMeta, in: FunctionExpMeta) extends FunctionExp {
  override val children = Seq(Seq(namedExpr), Seq(in))

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (newchildren.length != 2 || !(newchildren forall (_.length == 1)))
      throw new ClassCastException

    val newexp1: FunctionExpMeta = newchildren(0).head match {
      case e: FunctionExpMeta => e
      case _                  => throw new ClassCastException
    }
    val newexp2: FunctionExpMeta = newchildren(1).head match {
      case e: FunctionExpMeta => e
      case _                  => throw new ClassCastException
    }
    FunctionExpLet(name, newexp1, newexp2)
  }

  override def prettyPrint(writer: PrettyPrintWriter) = {
    writer.write("let ", name, " = ").write(namedExpr).write(" in ")
    writer.indentOptional().write(in).unindent()
  }

  override def toString() = s"let ${name} = (${namedExpr}) in (${in})"
}

final case class FunctionExpApp(functionName: String, args: Seq[FunctionExpMeta]) extends FunctionExp {
  override val children = Seq(args)

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (newchildren.length != 1)
      throw new ClassCastException

    val newargs: Seq[FunctionExpMeta] = newchildren(0) map {
      case e: FunctionExpMeta => e
      case _                  => throw new ClassCastException
    }
    FunctionExpApp(functionName, newargs)
  }

  override def prettyPrint(writer: PrettyPrintWriter) = {
    // NOTE the parenthesis are necessary! They distinguish an empty-args function application (e.g.
    // "something()" from a variable, bound by the pattern on the left side of a FunctionEq (just
    // "something")
    writer.write(functionName, "(")
    args.dropRight(1) foreach (writer.write(_).write(", "))
    args.lastOption foreach (writer.write(_))
    writer.write(")")
  }

  override def toString() = s"${functionName}(${args.mkString(",")})"
}

final case class FunctionMeta(metavar: MetaVar) extends FunctionExpMeta with SimplePrettyPrintable {
  override val children = Seq(Seq(metavar))

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (newchildren.length != 1 || !(newchildren forall (_.length == 1)))
      throw new ClassCastException

    val newmeta: MetaVar = newchildren(0).head match {
      case m: MetaVar => m
      case _          => throw new ClassCastException
    }
    FunctionMeta(newmeta)
  }

  override def prettyString = metavar.toPrettyString

  override def toString() = s"~${metavar.name}"
}

/**
 * convenience, such that MetaVars don't have to be explicitly wrapped when used inside as FunctionExp
 */
object FunctionMeta {
  implicit def wrap(metavar: MetaVar): FunctionMeta = FunctionMeta(metavar)
}

final case class FunctionExpVar(name: String) extends FunctionExp with SimplePrettyPrintable {
  override val children = Seq()

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (!newchildren.isEmpty) throw new ClassCastException

    //create a copy of myself
    FunctionExpVar(name)
  }

  override def prettyString = name

  override def toString() = name
}

final case object FunctionExpTrue extends FunctionExp with SimplePrettyPrintable {
  override val children = Seq()

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (!newchildren.isEmpty) throw new ClassCastException

    //return myself
    FunctionExpTrue
  }

  override val prettyString = "true"
  override def toString() = "true"
}

final case object FunctionExpFalse extends FunctionExp with SimplePrettyPrintable {
  override val children = Seq()

  override def transformChildren(newchildren: Seq[Seq[VeritasConstruct]]): VeritasConstruct = {
    if (!newchildren.isEmpty) throw new ClassCastException

    //return myself
    FunctionExpFalse
  }

  override val prettyString = "false"
  override def toString() = "false"
}

object FunctionExpMeta {
  def from(term: StrategoTerm): FunctionExpMeta = term match {
    case StrategoAppl("FunctionMeta", metavar) => FunctionMeta(MetaVar.from(metavar))
    case sa                                    => FunctionExp.from(sa) //if not a meta variable, try to parse as FunctionExp (will throw error if it doesn't work)
  }
}

object FunctionExp {
  def from(term: StrategoTerm): FunctionExp = term match {
    case StrategoAppl("FunctionExpNot", f) => FunctionExpNot(FunctionExp.from(f))
    case StrategoAppl("FunctionExpEq", f1, f2) => FunctionExpEq(FunctionExpMeta.from(f1), FunctionExpMeta.from(f2))
    case StrategoAppl("FunctionExpNeq", f1, f2) => FunctionExpNeq(FunctionExpMeta.from(f1), FunctionExpMeta.from(f2))
    case StrategoAppl("FunctionExpAnd", f1, f2) => FunctionExpAnd(FunctionExp.from(f1), FunctionExp.from(f2))
    case StrategoAppl("FunctionExpOr", f1, f2) => FunctionExpOr(FunctionExp.from(f1), FunctionExp.from(f2))
    case StrategoAppl("FunctionExpBiImpl", f1, f2) => FunctionExpBiImpl(FunctionExp.from(f1), FunctionExp.from(f2))
    case StrategoAppl("FunctionExpIf", cond, f1, f2) => FunctionExpIf(FunctionExpMeta.from(cond), FunctionExpMeta.from(f1), FunctionExpMeta.from(f2))
    case StrategoAppl("FunctionExpLet", StrategoString(name), namedExpr, in) => FunctionExpLet(name, FunctionExpMeta.from(namedExpr), FunctionExpMeta.from(in))
    case StrategoAppl("FunctionExpApp", StrategoString(func), StrategoList(args)) => FunctionExpApp(func, args map FunctionExpMeta.from)
    case StrategoAppl("FunctionExpVar", StrategoString(name)) => FunctionExpVar(name)
    case StrategoAppl("FunctionExpTrue") => FunctionExpTrue
    case StrategoAppl("FunctionExpFalse") => FunctionExpFalse
    case t => throw VeritasParseError(t)
  }
}
