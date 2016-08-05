package de.tu_darmstadt.veritas.inputdsl

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import java.io.PrintWriter

object Test {
  
  def main(args: Array[String]) {
    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    outputPrettyPrinter.writeln()
    outputPrettyPrinter.writeln("Here is the test module:")
    ExampleSpecification.natmoduleRaw.prettyPrint(outputPrettyPrinter)
    outputPrettyPrinter.close()
  }

}