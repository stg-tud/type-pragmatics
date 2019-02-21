package de.tu_darmstadt.veritas.lemmagen

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction.GraphConstructor
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.SimpleLemmaPrinter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import org.scalatest.FunSuite

class SQLCleverLemmaGenerationTest extends FunSuite {
  //val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
  val Directory = new File("generated")
  val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/SQLSpecAnnotated.scala")
  val generationProblem = new Problem(file)

  private def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }

  if(Directory.exists())
    recursivedelete(Directory)

  val generator = new AbstractLemmaGenerator {
    override def problem: Problem = generationProblem
    override def makePipeline(constructor: GraphConstructor): LemmaGeneratorPipeline = {
      new DefaultGeneratorPipeline with VisualizingGeneratorPipeline {
        override def directory: File = Directory
        override def problem: Problem = generationProblem
        override def graphConstructor: GraphConstructor = constructor
      }
    }
  }

  def printRules(lemmas: Seq[Lemma]) = {
    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    val lemmaPrettyPrinter = new SimpleLemmaPrinter {
      override val printer: PrettyPrintWriter = outputPrettyPrinter
    }
    lemmas.foreach { lemma =>
      lemmaPrettyPrinter.printTypingRule(lemma)
    }
    outputPrettyPrinter.flush()
  }

  for(func <- generator.progressFunctions) {
    lazy val lemmas = generator.generateProgressLemmas(func).toSeq
    val expectedLemmas = generationProblem.dsk.progressProperties.getOrElse(func, Seq())
    for(expected <- expectedLemmas) {
      test(s"progress ${func.signature.name} (${expected.name})") {
        println(s"===== ${lemmas.size} lemmas!")
        printRules(lemmas)
        println("")
        val equivalentLemmas = lemmas.filter(entry => LemmaEquivalence.isEquivalent(expected, entry))
        println(s"Equivalent to ${expected.name}: ${equivalentLemmas.length} out of ${lemmas.length}")
        assert(equivalentLemmas.nonEmpty)
      }
    }
    if(expectedLemmas.isEmpty)
      test(s"progress ${func.signature.name}") {
        println(s"===== ${lemmas.size} lemmas!")
        printRules(lemmas)
        println("")
        succeed
      }
  }

  for(func <- generator.preservationFunctions) {
    lazy val lemmas = generator.generatePreservationLemmas(func).toSeq
    val expectedLemmas = generationProblem.dsk.preservationProperties.getOrElse(func, Seq())
    for (expected <- expectedLemmas) {
      test(s"preservation ${func.signature.name} (${expected.name})") {
        println(s"===== ${lemmas.size} lemmas!")
        printRules(lemmas)
        println("")
        val equivalentLemmas = lemmas.filter(entry => LemmaEquivalence.isEquivalent(expected, entry))
        println(s"Equivalent to ${expected.name}: ${equivalentLemmas.length} out of ${lemmas.length}")
        assert(equivalentLemmas.nonEmpty)
      }
    }
    if (expectedLemmas.isEmpty)
      test(s"preservation ${func.signature.name}") {
        println(s"===== ${lemmas.size} lemmas!")
        printRules(lemmas)
        println("")
        succeed
      }
  }
}
