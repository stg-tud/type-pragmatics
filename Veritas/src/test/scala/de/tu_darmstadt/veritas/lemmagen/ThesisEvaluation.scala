package de.tu_darmstadt.veritas.lemmagen

import java.io._

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.CleverLemmaGenerator
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.evaluation.GeneratorEvaluator
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive.NaiveLemmaGenerator


object ThesisEvaluation {
  val EvaluationDirectory = new File("evaluation")

  def SQLSpec() = new Problem(new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala"))
  def SQLSpecAnnotated() = new Problem(new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/SQLSpecAnnotated.scala"))


/*  def writeBaselineLemmas(file: File): Unit = {
    val writer = new FileWriter(file)
    val latexWriter = new SimpleLemmaPrinter {
      override val printer: PrettyPrintWriter = new PrettyPrintWriter(writer)
    }
    for (lemma <- problem.dsk.properties) {
      writer.write(lemma.name + "\n")
      writer.write("--------------\n")
      latexWriter.printTypingRule(lemma)
      writer.write("\n")
      writer.flush()
    }
    writer.close()
  }


  def writeLemmaClasses(): Unit = {
    val sorted = problem.dsk.properties.toSeq.sortBy(_.name)
    val (progress, preservation) = sorted.partition(lemma =>
      problem.dsk.progressProperties.values.exists(_.contains(lemma)))
    val progressNames = progress.map(l => s"\\C{${l.name}}")
    val preservationNames = preservation.map(l => s"\\C{${l.name}}")
    val names = s"""
         | progress lemma & ${progressNames.mkString(", ")} \\\\
         | preservation lemma & ${preservationNames.mkString(", ")} \\\\
       """.stripMargin
    printToFile(new File(Directory, "lemma-classes.tex"), names)
  }

  def writeStaticFunctions(): Unit = {
    val names = sortFunctions(problem.dsk.staticFunctions.toSeq).map(formatFunctionName).mkString(", ")
    printToFile(new File(Directory, "static-functions.tex"), names)
  }

  def writeDynamicFunctions(): Unit = {
    val names = sortFunctions(problem.dsk.dynamicFunctions.toSeq).map(formatFunctionName).mkString(", ")
    printToFile(new File(Directory, "dynamic-functions.tex"), names)
  }*/

  def evaluateGenerator(problem: Problem, generator: Problem => LemmaGenerator, name: String): Unit = {
    val directory = new File(EvaluationDirectory, name)
    val storeFile = new File(EvaluationDirectory, s"lemmas-$name.dat")
    val evaluator = new GeneratorEvaluator(problem, generator(problem), storeFile, directory)
    println(s"evaluating $name ...")
    evaluator.evaluate()
  }

  def main(args: Array[String]): Unit = {
    evaluateGenerator(SQLSpec(), p => new NaiveLemmaGenerator(p), "naive")
    evaluateGenerator(SQLSpec(), p => new CleverLemmaGenerator(p), "clever-no-hints")
    evaluateGenerator(SQLSpecAnnotated(), p => new CleverLemmaGenerator(p), "clever-with-hints")
  }
}
