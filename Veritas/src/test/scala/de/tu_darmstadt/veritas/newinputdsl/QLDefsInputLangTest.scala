package de.tu_darmstadt.veritas.newinputdsl

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

object QLDefsInputLangTest {
  def main(args: Array[String]) {
    val translator = new SPLTranslator()

    val file = new File("src/test/scala/de/tu_darmstadt/veritas/newinputdsl/QLSpec.scala")

    try {
      val translated = translator.translate(file)
      val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
      outputPrettyPrinter.writeln()
      outputPrettyPrinter.writeln("Here are the test modules:")
      translated.prettyPrint(outputPrettyPrinter)
    } catch {
        case e: SPLTranslationError => println(e.msg)
    }

  }
}
