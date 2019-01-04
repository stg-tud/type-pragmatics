package de.tu_darmstadt.veritas.lemmagen

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Problem

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

  def main(args: Array[String]): Unit = {
    cleanDirectory()

  }
}
