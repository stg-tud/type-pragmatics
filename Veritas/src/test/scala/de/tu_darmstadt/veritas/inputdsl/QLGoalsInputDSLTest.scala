package de.tu_darmstadt.veritas.inputdsl

import java.io.PrintWriter

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

object QLGoalsInputDSLTest {

  def main(args: Array[String]) {
    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    outputPrettyPrinter.writeln()
    outputPrettyPrinter.writeln("Here are the test modules:")
    QLDefsTestGoals.CounterExamples.prettyPrint(outputPrettyPrinter)
    QLDefsTestGoals.Executions.prettyPrint(outputPrettyPrinter)
    QLDefsTestGoals.Proofs.prettyPrint(outputPrettyPrinter)
    QLDefsTestGoals.Syntheses.prettyPrint(outputPrettyPrinter)
    QLDefsTestGoals.Tests.prettyPrint(outputPrettyPrinter)
    outputPrettyPrinter.close()
  }
}
