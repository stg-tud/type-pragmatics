package de.tu_darmstadt.veritas.lemmagen

import java.io.{File, FileWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaEquivalence, Problem}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive.NaiveLemmaGenerator
import de.tu_darmstadt.veritas.backend.ast.TypingRule
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.prettyprint.SimpleToScalaSPLSpecificationPrinter

object NaiveEvaluation {
  val Directory = new File("evaluation-naive")
  val SpecFile = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala")
  val problem = new Problem(SpecFile)

  def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }

  def cleanDirectory(): Unit = {
    if (Directory.exists()) recursivedelete(Directory)
    Directory.mkdirs()
  }

  def generate(): Map[FunctionDef, Set[Lemma]] = {
    val generator = new NaiveLemmaGenerator(problem)
    generator.generate()
  }

  def writeLemmas(file: File, lemmas: Set[Lemma]): Unit = {
    println(s"Writing ${lemmas.size} lemmas to $file ...")
    val writer = new FileWriter(file)
    val lemmaWriter = new SimpleToScalaSPLSpecificationPrinter {
      override val printer: PrettyPrintWriter = new PrettyPrintWriter(writer)
    }
    for (lemma <- lemmas) {
      writer.write("--------------\n")
      lemmaWriter.printTypingRule(lemma)
      writer.write("\n")
      writer.write("Refinements:\n")
      for (refinement <- lemma.refinements) {
        writer.write("  " + refinement + "\n")
      }
      writer.flush()
    }
    writer.close()
  }

  def lookupExpectedLemmas(fn: FunctionDef): Set[TypingRule] = {
    problem.dsk.lookupByFunName(
      problem.dsk.progressProperties,
      fn.signature.name).toSet ++ problem.dsk.lookupByFunName(
      problem.dsk.preservationProperties,
      fn.signature.name
    )
  }

  def evaluate(result: Map[FunctionDef, Set[Lemma]]): Unit = {
    result.foreach {
      case (function, lemmas) =>
        val name = function.signature.name
        println(s"evaluating $name...")
        // all lemmas
        writeLemmas(new File(Directory, s"generated-$name.txt"), lemmas)
        // equivalent lemmas
        val expectedLemmas = lookupExpectedLemmas(function)
        val equivalentLemmas = lemmas.filter(generated =>
          expectedLemmas.exists(expected =>
            LemmaEquivalence.isEquivalent(expected, generated)
          )
        )
        writeLemmas(new File(Directory, s"equivalent-$name.txt"), equivalentLemmas)
    }
  }

  def main(args: Array[String]): Unit = {
    cleanDirectory()
    val lemmas = generate()
    evaluate(lemmas)
  }
}
