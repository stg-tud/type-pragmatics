package de.tu_darmstadt.veritas.scalaspl.prettyprint

import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference.Sort
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledge

// TODO what about dsk Annotations? Generics to support them?
trait ToScalaSPLSpecificationPrinter {
  val printer: PrettyPrintWriter

  def print(module: Module): String = {
    printer.writeln(s"trait ${module.name} extends ScalaSPLSpecification {")
    printer.indent()
    module.defs.foreach {
      case dt: DataType =>
        printDataType(dt)
        printer.writeln()
      case Functions(funcs) => funcs.foreach { fn =>
        printFunction(fn)
        printer.writeln()
      }
      case PartialFunctions(funcs) => funcs.foreach { fn =>
        printer.writeln("@Partial")
        printFunction(fn)
        printer.writeln()
      }
      case Axioms(axioms) => axioms.foreach { axiom =>
        printer.writeln("@Axiom")
        printTypingRule(axiom)
        printer.writeln()
      }
      case Lemmas(lemmas, _) => lemmas.foreach { lemma =>
        printer.writeln("@Lemma")
        printTypingRule(lemma)
        printer.writeln()
      }
      case Goals(goals, _) => goals.foreach { goal =>
        printer.writeln("@Goal")
        printTypingRule(goal)
        printer.writeln()
      }
      case _ => ()
    }
    printer.unindent()
    printer.writeln("}")
    printer.flush()
    val res = printer.toString()
    printer.close()
    res
  }

  def printDataType(dt: DataType): Unit

  def printParamList(params: Seq[SortRef]): Unit = {
    if (params.nonEmpty) {
      for ((param, pos) <- params.init.zipWithIndex) {
        printer.write(s"a${pos}: ${param.name}, ")
      }
      printer.write(s"a${params.size - 1}: ${params.last}")
    }
  }

  def printFunction(fun: FunctionDef): Unit = {
    val eqs = fun.eqn
    printSignature(fun.signature)
    if (eqs.nonEmpty) {
      // TODO print match
      printMatchTuple(fun.signature.in.size)
      printer.write(" match {")
      printer.indent()
      for (eq <- eqs) {
        printEquation(eq)
      }
      printer.unindent()
      printer.writeln("}")
    } else {
      printer.writeln("???")
    }
  }

  def printMatchTuple(size: Int): Unit = {
    printer.write("(")
    for (i <- 0 until size - 1) {
      printer.write(s"a${i}, ")
    }
    printer.write(s"a${size - 1})")
  }

  def printSignature(sig: FunctionSig): Unit = {
    printer.write(s"def ${sig.name}(")
    printParamList(sig.in)
    printer.write(s"): ${sig.out.name} = ")
  }

  def printEquation(eq: FunctionEq): Unit = {
    printer.write("case ")
    printPatterns(eq.patterns)
    printer.write(" => ")
    printer.indent()
    printFunctionExp(eq.right)
    printer.unindent()
    printer.newline()
  }

  def printPatterns(patterns: Seq[FunctionPattern]): Unit = {
    printer.write("(")
    if (patterns.nonEmpty) {
      patterns.init.foreach { pattern =>
        printPattern(pattern)
        printer.write(", ")
      }
      printPattern(patterns.last)
    }
    printer.write(")")
  }

  def printPattern(pattern: FunctionPattern): Unit = pattern match {
    case FunctionPatApp(name, args) =>
      printer.write(name)
      printer.write("(")
      if (args.nonEmpty) {
        args.init.foreach { arg =>
          printPattern(arg)
          printer.write(", ")
        }
        printer.write(args.last)
      }
      printer.write(")")
    case FunctionPatVar(name) => printer.write(name)
  }

  def printFunctionExp(meta: FunctionExpMeta): Unit = meta match {
    case FunctionMeta(MetaVar(name)) =>
      printer.write(name)
    case fexp: FunctionExp => printFunctionExp(fexp)
  }

  def printFunctionExp(exp: FunctionExp): Unit = exp match {
    case FunctionExpNot(f) =>
      printer.write("!")
      printFunctionExp(f)
    case FunctionExpEq(lhs, rhs) =>
      printBinOpFunctionExp("==", lhs, rhs)
    case FunctionExpNeq(lhs, rhs) =>
      printBinOpFunctionExp("!=", lhs, rhs)
    case FunctionExpAnd(lhs, rhs) =>
      printBinOpFunctionExp("&&", lhs, rhs)
    case FunctionExpOr(lhs, rhs) =>
      printBinOpFunctionExp("||", lhs, rhs)
    case FunctionExpBiImpl(lhs, rhs) =>
      printBinOpFunctionExp("<==>", lhs, rhs)
    case FunctionExpIf(cond, els, thn) =>
      printFunctionExpIf(cond, els, thn)
    case FunctionExpLet(name, namedExpr, in) =>
      printFunctionExpLet(name, namedExpr, in)
    case FunctionExpApp(name, args) =>
      printFunctionExpApp(name, args)
    case FunctionExpVar(name) => printer.write(name)
    case FunctionExpTrue => printer.write("true")
    case FunctionExpFalse => printer.write("false")
  }

  def printBinOpFunctionExp(op: String, lhs: FunctionExpMeta, rhs: FunctionExpMeta): Unit = {
    printFunctionExp(lhs)
    printer.write(s" ${op} ")
    printFunctionExp(rhs)
  }

  def printFunctionExpIf(cond: FunctionExp, els: FunctionExpMeta, thn: FunctionExpMeta): Unit = {
    printer.write("if(")
    printFunctionExp(cond)
    printer.write(") {")
    printer.indent()
    printFunctionExp(els)
    printer.unindent()
    printer.newline()
    printer.write("} else {")
    printer.indent()
    printFunctionExp(thn)
    printer.unindent()
    printer.newline()
    printer.write("}")
  }

  def printFunctionExpLet(name: String, namedExpr: FunctionExpMeta, in: FunctionExpMeta): Unit = {
    printer.write(s"val ${name} = ")
    printFunctionExp(namedExpr)
    printer.writeln()
    printFunctionExp(in)
  }

  def printFunctionExpApp(name: String, args: Seq[FunctionExpMeta]): Unit = {
    printer.write(s"${name}(")
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
    printer.write(s"def ${tr.name}(")
    val metaVars: Set[MetaVar] = collectMetaVars(tr)
    printBindings(metaVars.toSeq, tr)
    printer.write("): Unit = {")
    printer.indent()
    tr.premises.foreach { pr =>
      printer.write("require(")
      printTypingRuleJudgement(pr)
      printer.writeln(")")
    }
    printer.unindent()
    printer.write("} ensuring (")
    tr.consequences.init.foreach { cons =>
      printTypingRuleJudgement(cons)
      printer.write(" && ")
    }
    printTypingRuleJudgement(tr.consequences.last)
    printer.writeln(")")
  }

  def printTypingRuleJudgement(trj: TypingRuleJudgment): Unit = trj match {
    case TypingJudgment(ctx, exp, typ) =>
      printFunctionExp(ctx)
      printer.write(" |- ")
      printFunctionExp(exp)
      printer.write(" :: ")
      printFunctionExp(typ)
    case TypingJudgmentSimple(exp, typ) =>
      printFunctionExp(exp)
      printer.write(" :: ")
      printFunctionExp(typ)
    case FunctionExpJudgment(f) => printFunctionExp(f)
    case ExistsJudgment(bindings, body) =>
      printQuantifier("exists", bindings, body)
    case ForallJudgment(bindings, body) =>
      printQuantifier("forall", bindings, body)
    case OrJudgment(cases) =>
      cases.init.foreach { outer =>
        printInnerOrJudgment(outer)
        printer.write(" && ")
      }
      printInnerOrJudgment(cases.last)
  }

  def printQuantifier(name: String, bindings: Seq[MetaVar], body: Seq[TypingRuleJudgment]): Unit = {
    printer.write(s"${name}((")
    printBindings(bindings, null)
    printer.write(") => ")
    if (body.nonEmpty) {
      body.init.foreach { trj =>
        printTypingRuleJudgement(trj)
        printer.write(" && ")
      }
      printTypingRuleJudgement(body.last)
    }
    printer.write(")")
  }

  def printBindings(bindings: Seq[MetaVar], tr: TypingRule): Unit

  def printInnerOrJudgment(cases: Seq[TypingRuleJudgment]): Unit = {
    cases.init.foreach { c =>
      printTypingRuleJudgement(c)
      printer.write(" || ")
    }
    printTypingRuleJudgement(cases.last)
  }

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

trait SimpleToScalaSPLSpecificationPrinter extends ToScalaSPLSpecificationPrinter {
  def printDataType(dt: DataType): Unit = {
    if (!dt.open) {
      printer.write("sealed ")
    }
    printer.writeln(s"trait ${dt.name} extends ???")
    for (ctor <- dt.constrs) {
      printer.write(s"case class ${ctor.name}(")
      printParamList(ctor.in)
      printer.writeln(s") extends ${dt.name}")
    }
  }

  def getBindingType(binding: MetaVar): String = binding.typ match {
    case Some(Sort(name)) => name
    case _ => "Any"
  }

  override def printBindings(bindings: Seq[MetaVar], tr: TypingRule): Unit = {
    if (bindings.nonEmpty) {
      bindings.init.foreach { binding =>
        printer.write(binding.name)
        printer.write(s": ${getBindingType(binding)}, ")
      }
      printer.write(bindings.last.name)
      printer.write(s": ${getBindingType(bindings.last)}")
    }
  }
}
