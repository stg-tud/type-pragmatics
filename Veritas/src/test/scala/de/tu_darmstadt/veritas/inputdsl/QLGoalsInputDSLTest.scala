package de.tu_darmstadt.veritas.inputdsl

import java.io.PrintWriter

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

object QLGoalsInputDSLTest {

  def main(args: Array[String]) {
    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    outputPrettyPrinter.writeln()
    outputPrettyPrinter.writeln("Here are the test modules:")
    QLDefs.CounterExamples.prettyPrint(outputPrettyPrinter)
    QLDefs.Executions.prettyPrint(outputPrettyPrinter)
    QLDefs.Proofs.prettyPrint(outputPrettyPrinter)
    QLDefs.Syntheses.prettyPrint(outputPrettyPrinter)
    QLDefs.Tests.prettyPrint(outputPrettyPrinter)
    outputPrettyPrinter.close()
  }
}
