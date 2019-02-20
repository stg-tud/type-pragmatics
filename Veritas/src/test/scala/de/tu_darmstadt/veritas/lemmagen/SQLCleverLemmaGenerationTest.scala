package de.tu_darmstadt.veritas.lemmagen

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.{CleverLemmaGenerator, PredicatePreservationConstructor, ProgressConstructor, ScalaSPLSpecificationOutput}
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.prettyprint.SimpleToScalaSPLSpecificationPrinter
import org.scalatest.FunSuite

import scala.collection.mutable
import scala.meta.inputs.Input

class SQLCleverLemmaGenerationTest extends FunSuite {
  //val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
  val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/SQLSpecAnnotated.scala")
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

  private val progressLemmas = new mutable.HashMap[FunctionDef, Seq[Lemma]]().withDefault(_ => Seq())
  private val preservationLemmas = new mutable.HashMap[FunctionDef, Seq[Lemma]]().withDefault(_ => Seq())

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
    test(s"add ${func.signature.name} progress lemmas to store") {
      progressLemmas(func) ++= lemmas
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
    test(s"add ${func.signature.name} preservation lemmas to store") {
      preservationLemmas(func) ++= lemmas
    }
  }

  test("write updated megaspec") {
    println(s"progress lemmas: ${progressLemmas.values.flatten.size}")
    println(s"preservation lemmas: ${preservationLemmas.values.flatten.size}")
    println("")
    val specString = scala.io.Source.fromFile(file).mkString("")
    val input = Input.VirtualFile(file.getAbsolutePath, specString)
    val updatedString = ScalaSPLSpecificationOutput.addLemmasToSpecification(input, progressLemmas.toMap, preservationLemmas.toMap)
    val updatedFile = new File("generated/SQLSpecUpdated.scala")
    val writer = new BufferedWriter(new FileWriter(updatedFile))
    writer.write(updatedString)
    writer.close()
    succeed
  }
}
