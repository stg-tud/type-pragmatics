package de.tu_darmstadt.veritas.inputdsl

import java.io.PrintWriter

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

object QLDefsInputDSLTest {

  def main(args: Array[String]) {
    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    outputPrettyPrinter.writeln()
    outputPrettyPrinter.writeln("Here are the test modules:")
    QLDefsSpec.BasicTypes.prettyPrint(outputPrettyPrinter)
    QLDefsSpec.QLSyntax.prettyPrint(outputPrettyPrinter)
    QLDefsSpec.QLSemanticsData.prettyPrint(outputPrettyPrinter)
    QLDefsSpec.QLSemantics.prettyPrint(outputPrettyPrinter)
    QLDefsTypeSystem.QLTypeSystem.prettyPrint(outputPrettyPrinter)
    QLDefsTypeSystem.QLTypeSystemInv.prettyPrint(outputPrettyPrinter)
    outputPrettyPrinter.close()
  }

}
