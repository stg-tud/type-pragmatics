package de.tu_darmstadt.veritas.scalaspl

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.translator.{ScalaSPLTranslationError, ScalaSPLTranslator}

object QLDefsInputLangTest {
  def main(args: Array[String]) {
    val translator = new ScalaSPLTranslator()

    val file = new File("src/test/scala/de/tu_darmstadt/veritas/newinputdsl/QLSpec.scala")

    try {
      val translated = translator.translate(file)
      val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
      outputPrettyPrinter.writeln()
      outputPrettyPrinter.writeln("Here are the test modules:")
      translated.prettyPrint(outputPrettyPrinter)
    } catch {
        case e: ScalaSPLTranslationError => println(e.msg)
    }

  }
}
