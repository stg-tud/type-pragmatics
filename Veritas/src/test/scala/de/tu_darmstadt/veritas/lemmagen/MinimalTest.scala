package de.tu_darmstadt.veritas.lemmagen

import java.io._

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Problem, ScalaSPLSpecificationOutput}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.CleverHintsLemmaGenerator

object MinimalTest {
  def main(args: Array[String]): Unit = {
    val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/SQLSpecAnnotated.scala")
    val problem = new Problem(file)
    val generator = new CleverHintsLemmaGenerator(problem)
    val updatedSpec = ScalaSPLSpecificationOutput.updateSpecification(problem, generator)
    println(updatedSpec)
  }
}

