package de.tu_darmstadt.veritas.lemmagen

import java.io.{File, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
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
/*
  def rename(lemmas: Seq[Lemma]): Seq[Lemma] = {
    (for((l, i) <- lemmas.toSeq.zipWithIndex) yield l.rename(s"Lemma$i")).toSet
  }*/

  Seq("findCol", "lookupStore", "projectTable", "projectCols").foreach { name =>
    test(s"generate $name progress") {
      val fn = problem.dsk.lookupByFunName(problem.dsk.dynamicFunctions, name).get
      val generator = new ProgressGenerator(problem, fn)
      val base = generator.generateBase().head
      val lemmas = generator.constrainConsequenceVariables(base)
      for (lemma <- lemmas) {
        lemmaPrettyPrinter.printTypingRule(lemma)
        outputPrettyPrinter.flush()
        println()
      }
      //println("prune ...")

      val pool = new ShapedPool()

      for(lemma <- lemmas) {
        generator.addEquations(lemma)
      }

      /*for (lemma <- lemmas) {
        val eqs = rename(generator.addEquations(lemma).toSeq).toSet
        val pruned = Oracle.pruneProvablyFalseLemmas(problem, eqs)
        for (lemma <- pruned) {
          pool.add(lemma)
        }
      }
      for(lemma <- pool.lemmas) {
        lemmaPrettyPrinter.printTypingRule(lemma)
        outputPrettyPrinter.flush()
        println(lemma.refinements)
        println()
      }*/

      val progressProperties = problem.dsk.lookupByFunName(problem.dsk.progressProperties, name)
      println(s"generated ${pool.lemmas.size} lemmas")
      progressProperties.foreach { expected =>
        val equivalent = pool.lemmas.filter(l => LemmaEquivalence.isEquivalent(expected, l))
        println(s"equivalent to ${expected.name}: ${equivalent.size}")
      }
    }
  }
}
