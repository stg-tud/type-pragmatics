package de.tu_darmstadt.veritas.lemmagen

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslator
import org.scalatest.FunSuite

class SQLLemmaGenerationTest extends FunSuite {
  test("Read @Static and @Dynamic annotations from SQLSpec") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
    val module = new ScalaSPLTranslator().translate(file)
    val dsk = DomainSpecificKnowledgeBuilder().build(file)

    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))

    def prettyPrintFunctions(funcs: Set[FunctionDef]): Unit = {
      outputPrettyPrinter.indent()
      for(fctn <- funcs) {
        fctn.signature.prettyPrint(outputPrettyPrinter)
        outputPrettyPrinter.newline()
      }
      outputPrettyPrinter.unindent()
    }
    println(s"${dsk.staticFunctions.size} static functions:")
    prettyPrintFunctions(dsk.staticFunctions)
    println()
    println(s"${dsk.dynamicFunctions.size} dynamic functions:")
    prettyPrintFunctions(dsk.dynamicFunctions)
    println()
    println(s"${dsk.predicates.size} predicates:")
    prettyPrintFunctions(dsk.predicates)
    println()
    println(s"${dsk.failableTypes.size} failable types:")
    println()
    for(ft <- dsk.failableTypes) {
      ft.prettyPrint(outputPrettyPrinter)
      outputPrettyPrinter.newline()
    }
    outputPrettyPrinter.flush()
  }
}
