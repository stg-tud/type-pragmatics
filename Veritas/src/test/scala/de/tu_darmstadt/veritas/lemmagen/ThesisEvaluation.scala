package de.tu_darmstadt.veritas.lemmagen

import java.io._

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.CleverLemmaGenerator
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.evaluation.{GeneralInformation, GeneratorEvaluator}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive.NaiveLemmaGenerator


object ThesisEvaluation {
  val EvaluationDirectory = new File("evaluation")

  def SQLSpec() = new Problem(new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala"))
  def SQLSpecAnnotated() = new Problem(new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/SQLSpecAnnotated.scala"))

  def evaluateGenerator(problem: Problem, generator: Problem => LemmaGenerator, name: String): Unit = {
    val directory = new File(EvaluationDirectory, name)
    val storeFile = new File(EvaluationDirectory, s"lemmas-$name.dat")
    val evaluator = new GeneratorEvaluator(problem, generator(problem), storeFile, directory)
    println(s"evaluating $name ...")
    evaluator.evaluate()
  }

  def writeGeneralInfo(problem: Problem): Unit = {
    println("writing general info ...")
    val general = new GeneralInformation(problem, new File(EvaluationDirectory, "general"))
    general.write()
  }

  def main(args: Array[String]): Unit = {
    evaluateGenerator(SQLSpec(), p => new NaiveLemmaGenerator(p), "naive")
    evaluateGenerator(SQLSpec(), p => new CleverLemmaGenerator(p), "clever-no-hints")
    evaluateGenerator(SQLSpecAnnotated(), p => new CleverLemmaGenerator(p), "clever-with-hints")
    writeGeneralInfo(SQLSpecAnnotated()) // TODO: annotated?
  }
}
