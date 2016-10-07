package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import java.io.PrintWriter

import de.tu_darmstadt.veritas.inputdsl.testexamples.{ExampleSpecification, SQLDefs}

object Test {
  
  def main(args: Array[String]) {
    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    outputPrettyPrinter.writeln()
    outputPrettyPrinter.writeln("Here are the test modules:")
    SQLDefs.Tables.prettyPrint(outputPrettyPrinter)
    SQLDefs.TableAux.prettyPrint(outputPrettyPrinter)
    SQLDefs.TStore.prettyPrint(outputPrettyPrinter)
    SQLDefs.TContext.prettyPrint(outputPrettyPrinter)
    SQLDefs.Syntax.prettyPrint(outputPrettyPrinter)
    SQLDefs.Semantics.prettyPrint(outputPrettyPrinter)
    outputPrettyPrinter.close()
  }

}