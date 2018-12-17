package de.tu_darmstadt.veritas.lemmagen

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Problem, SimpleLemmaPrinter}
import de.tu_darmstadt.veritas.backend.transformation.collect.{CollectTypesDefs, CollectTypesDefsClass}
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import org.scalatest.FunSuite

class ThesisTest extends FunSuite {
  test("write lemma") {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/ThesisExampleSpec.scala")
    val problem = new Problem(file)

    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    val lemmaPrettyPrinter = new SimpleLemmaPrinter {
      override val printer: PrettyPrintWriter = outputPrettyPrinter
    }
    problem.dsk.properties.foreach { prop =>
      problem.enquirer.getAllVarTypes(prop) // needed so that quantified variables have the right type
      lemmaPrettyPrinter.printTypingRule(prop)
    }
    problem.enquirer
    outputPrettyPrinter.flush()
  }
}
