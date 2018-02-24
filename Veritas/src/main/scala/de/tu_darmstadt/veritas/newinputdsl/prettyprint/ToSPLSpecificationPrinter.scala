package de.tu_darmstadt.veritas.newinputdsl.prettyprint

import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast.{DataType, Functions, Module, SortRef}
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

// TODO what about dsk Annotations? Generics to support them?
trait ToSPLSpecificationPrinter {
  val printer: PrettyPrintWriter

  def print(module: Module): String = {
    printer.writeln(s"trait ${module.name} extends SPLSpecification {")
    printer.indent()
    module.defs.foreach {
      case dt: DataType =>
        printDataType(dt)
        printer.writeln()
      case Functions(funcs) => funcs.foreach { fn =>
        printFunction(fn)
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

  // TODO for every AST needs a function to translate it back. Could be that we need more than one
  // AST
  def printDataType(dt: DataType): Unit = {
    // TODO how do we know what category it belongs to (Expression, Context, Typ)?
    printer.writeln(s"trait ${dt.name} extends ???")
    for (ctor <- dt.constrs) {
      printer.write(s"case class ${ctor.name}(")
      printParamList(ctor.in)
      printer.writeln(s") extends ${dt.name}")
    }
  }

  def printParamList(params: Seq[SortRef]): Unit = {
    if (params.nonEmpty) {
      for ((param, pos) <- params.init.zipWithIndex) {
        printer.write(s"a${pos}: ${param.name}, ")
      }
      printer.write(s"a${params.size - 1}: ${params.last}")
    }
  }

  private def getArgNameForPosition(position: Int): Char = {
    val baseNumber = 97 // int of char a
    (baseNumber + position).asInstanceOf[Char]
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
    printer.writeln(" => ")
    printFunctionExp(eq.right)
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
      if (args.nonEmpty) {
        printer.write("(")
        args.init.foreach { arg =>
          printPattern(arg)
          printer.write(", ")
        }
        printer.write(args.last)
      }
      printer.write(")")
    // TODO how do i know that name is a function application (a empty ctor)?
    case FunctionPatVar(name) => printer.write(name)
  }

  def printPatternName(name: String): Unit = {

  }

  def printFunctionExp(right: FunctionExp): Unit = ()
  // TypingRules

  // TypingRuleJudgements
}
