package de.tu_darmstadt.veritas.lemmagen

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Oracle, Problem}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.ProgressGenerator
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive.ProgressStrategy
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.prettyprint.SimpleToScalaSPLSpecificationPrinter
import org.scalatest.FunSuite

class SQLCleverLemmaGenerationTest extends FunSuite {
  val file = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
  val problem = new Problem(file)

  val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
  val lemmaPrettyPrinter = new SimpleToScalaSPLSpecificationPrinter {
    override val printer: PrettyPrintWriter = outputPrettyPrinter
  }

  def rename(lemmas: Set[Lemma]): Set[Lemma] = {
    (for((l, i) <- lemmas.toSeq.zipWithIndex)
      yield new Lemma(s"Lemma$i", l.premises, l.consequences)).toSet
  }

  test("generate findCol progress") {
    val fn = problem.dsk.lookupByFunName(problem.dsk.dynamicFunctions, "findCol").get
    val generator = new ProgressGenerator(problem, fn)
    val base = generator.generateBase().head
    val lemmas = rename(generator.constrainConsequenceVariables(base))
    for (lemma <- lemmas) {
      lemmaPrettyPrinter.printTypingRule(lemma)
      outputPrettyPrinter.flush()
      println()
    }
    println("prune ...")

    for (lemma <- lemmas) {
      val eqs = rename(generator.addEquations(lemma))
      val pruned = Oracle.pruneProvablyFalseLemmas(problem, eqs)
      for (lemma <- pruned) {
        lemmaPrettyPrinter.printTypingRule(lemma)
        outputPrettyPrinter.flush()
        println()
      }
    }
  }
}
