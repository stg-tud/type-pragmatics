package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference.Sort
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

// TODO what about dsk Annotations? Generics to support them?
trait LemmaPrinter {
  val printer: PrettyPrintWriter

  def print(module: Module): String = {
    module.defs.foreach {
      case Lemmas(lemmas, _) => lemmas.foreach { lemma =>
        printTypingRule(lemma)
        printer.writeln()
      }
      case Goals(goals, _) => goals.foreach { goal =>
        printTypingRule(goal)
        printer.writeln()
      }
      case _ => ()
    }
    printer.flush()
    val res = printer.toString()
    printer.close()
    res
  }

  def printFunctionExp(meta: FunctionExpMeta): Unit = meta match {
    case FunctionMeta(MetaVar(name)) =>
      printer.write(translateVariableName(name))
    case fexp: FunctionExp => printFunctionExp(fexp)
  }

  def translateVariableName(str: String): String = {
    val (anything, numbers) = str.span(!_.isDigit)
    val inner = if(numbers.nonEmpty && (numbers forall Character.isDigit)) {
      anything + "_" + numbers
    } else {
      str
    }
    s"\\I{$inner}"
  }

  def printFunctionExp(exp: FunctionExp): Unit = exp match {
    case FunctionExpNot(f) =>
      printer.write("!")
      printFunctionExp(f)
    case FunctionExpEq(lhs, rhs) =>
      printBinOpFunctionExp("=", lhs, rhs)
    case FunctionExpNeq(lhs, rhs) =>
      printBinOpFunctionExp("\\neq", lhs, rhs)
    case FunctionExpAnd(lhs, rhs) =>
      printBinOpFunctionExp("\\wedge", lhs, rhs)
    case FunctionExpOr(lhs, rhs) =>
      printBinOpFunctionExp("\\vee", lhs, rhs)
    /*case FunctionExpBiImpl(lhs, rhs) =>
      printBinOpFunctionExp("\\leftrightarrow", lhs, rhs)*/
    case FunctionExpApp(name, args) =>
      printFunctionExpApp(name, args)
    case FunctionExpVar(name) => printer.write(name)
    case FunctionExpTrue => printer.write("\\textsf{true}")
    case FunctionExpFalse => printer.write("\\textsf{false}")
  }

  def printBinOpFunctionExp(op: String, lhs: FunctionExpMeta, rhs: FunctionExpMeta): Unit = {
    printFunctionExp(lhs)
    printer.write(s" ${op} ")
    printFunctionExp(rhs)
  }

  def printFunctionExpApp(name: String, args: Seq[FunctionExpMeta]): Unit = {
    printer.write(s"\\textsf{${name}}(")
    if (args.nonEmpty) {
      args.init.foreach { arg =>
        printFunctionExp(arg)
        printer.write(", ")
      }
      printFunctionExp(args.last)
    }
    printer.write(")")
  }

  def printTypingRule(tr: TypingRule): Unit = {
    printer.writeln("\\begin{prooftree}")
    /*val metaVars: Set[MetaVar] = collectMetaVars(tr)
    printBindings(metaVars.toSeq, tr)
    printer.write("): Unit = {")*/
    printer.writeln("\\alwaysNoLine")
    if(tr.premises.nonEmpty) {
      printer.write("\\AxiomC{$")
      printTypingRuleJudgement(tr.premises.head)
      printer.writeln("$}")
      tr.premises.tail.foreach { pr =>
        printer.write("\\UnaryInfC{$")
        printTypingRuleJudgement(pr)
        printer.writeln("$}")
      }
    } else {
      printer.write("\\AxiomC{}")
    }
    printer.writeln("\\alwaysSingleLine")
    printer.write("\\UnaryInfC{$")
    require(tr.consequences.length == 1) // TODO
    printTypingRuleJudgement(tr.consequences.head)
    printer.writeln("$}")
    printer.writeln("\\end{prooftree}")
  }

  def printTypingRuleJudgement(trj: TypingRuleJudgment): Unit = trj match {
    /*case TypingJudgment(ctx, exp, typ) =>
      printFunctionExp(ctx)
      printer.write(" |- ")
      printFunctionExp(exp)
      printer.write(" :: ")
      printFunctionExp(typ)
    case TypingJudgmentSimple(exp, typ) =>
      printFunctionExp(exp)
      printer.write(" :: ")
      printFunctionExp(typ)*/
    case TypingJudgment(ctx, exp, typ) =>
      printFunctionExp(ctx)
      printer.write("\\vdash")
      printFunctionExp(exp)
      printer.write("::")
      printFunctionExp(typ)
    case FunctionExpJudgment(f) => printFunctionExp(f)
    case ExistsJudgment(bindings, body) =>
      printQuantifier("\\exists", bindings, body)
  }

  def printQuantifier(name: String, bindings: Seq[MetaVar], body: Seq[TypingRuleJudgment]): Unit = {
    printer.write(s"$name ")
    printBindings(bindings, null)
    printer.write("\\ldotp")
    if (body.nonEmpty) {
      body.init.foreach { trj =>
        printTypingRuleJudgement(trj)
        printer.write(" \\wedge ")
      }
      printTypingRuleJudgement(body.last)
    }
  }

  def printBindings(bindings: Seq[MetaVar], tr: TypingRule): Unit

  /*def printInnerOrJudgment(cases: Seq[TypingRuleJudgment]): Unit = {
    cases.init.foreach { c =>
      printTypingRuleJudgement(c)
      printer.write(" || ")
    }
    printTypingRuleJudgement(cases.last)
  }*/

  def collectMetaVars(tr: TypingRule): Set[MetaVar] = (tr.consequences ++ tr.premises).flatMap { collectMetaVars }.toSet

  def collectMetaVars(trj: TypingRuleJudgment): Set[MetaVar] = trj match {
    case TypingJudgment(ctx, exp, typ) => collectMetaVars(ctx, Set()) ++ collectMetaVars(exp, Set()) ++ collectMetaVars(typ, Set())
    case TypingJudgmentSimple(exp, typ) => collectMetaVars(exp, Set()) ++ collectMetaVars(typ, Set())
    case FunctionExpJudgment(f) => collectMetaVars(f, Set())
    case ExistsJudgment(bindings, body) => body.flatMap(collectMetaVars).diff(bindings).toSet
    case ForallJudgment(bindings, body) => body.flatMap(collectMetaVars).diff(bindings).toSet
    case OrJudgment(cases) => cases.flatMap(_.flatMap(collectMetaVars)).toSet
  }

  def collectMetaVars(f: FunctionExpMeta, acc: Set[MetaVar]): Set[MetaVar] = f match {
    case FunctionMeta(mv) => acc + mv
    case FunctionExpNot(f) => collectMetaVars(f, acc)
    case FunctionExpBiImpl(lhs, rhs) => collectMetaVars(lhs, acc) ++ collectMetaVars(rhs, acc)
    case FunctionExpAnd(lhs, rhs) => collectMetaVars(lhs, acc) ++ collectMetaVars(rhs, acc)
    case FunctionExpOr(lhs, rhs) => collectMetaVars(lhs, acc) ++ collectMetaVars(rhs, acc)
    case FunctionExpEq(lhs, rhs) => collectMetaVars(lhs, acc) ++ collectMetaVars(rhs, acc)
    case FunctionExpNeq(lhs, rhs) => collectMetaVars(lhs, acc) ++ collectMetaVars(rhs, acc)
    case FunctionExpApp(_, args) => args.flatMap { arg => collectMetaVars(arg, acc) }.toSet
    case FunctionExpIf(cond, els, thn) => collectMetaVars(cond, acc) ++ collectMetaVars(els, acc) ++ collectMetaVars(thn, acc)
    case FunctionExpLet(_, named, in) => collectMetaVars(named, acc) ++ collectMetaVars(in, acc)
    case _ => acc
  }
}

trait SimpleLemmaPrinter extends LemmaPrinter {
  def getBindingType(binding: MetaVar): String = binding.typ match {
    case Some(Sort(name)) => s"\\textsf{$name}"
    case _ => "\\textsf{Any}"
  }

  override def printBindings(bindings: Seq[MetaVar], tr: TypingRule): Unit = {
    if (bindings.nonEmpty) {
      bindings.init.foreach { binding =>
        printer.write(translateVariableName(binding.name))
        printer.write(s"\\colon ${getBindingType(binding)}, ")
      }
      printer.write(translateVariableName(bindings.last.name))
      printer.write(s"\\colon ${getBindingType(bindings.last)}")
    }
  }
}
