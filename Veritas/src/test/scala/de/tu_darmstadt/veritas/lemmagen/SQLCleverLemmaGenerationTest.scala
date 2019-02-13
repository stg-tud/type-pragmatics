package de.tu_darmstadt.veritas.lemmagen

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.{CleverLemmaGenerator, PredicatePreservationConstructor, ProgressConstructor}
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import org.scalatest.FunSuite

class SQLCleverLemmaGenerationTest extends FunSuite {
  val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
  val problem = new Problem(file)
  val generator = new CleverLemmaGenerator(problem)

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
    val expectedLemmas = problem.dsk.progressProperties.getOrElse(func, Seq())
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
    val expectedLemmas = problem.dsk.preservationProperties.getOrElse(func, Seq())
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
