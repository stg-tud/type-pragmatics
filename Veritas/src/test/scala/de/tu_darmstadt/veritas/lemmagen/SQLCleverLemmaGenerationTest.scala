package de.tu_darmstadt.veritas.lemmagen

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.construction.GraphConstructor
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever._
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.SimpleLaTeXLemmaPrinter
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import org.scalatest.FunSuite

import scala.collection.mutable
import scala.meta.inputs.Input

class SQLCleverLemmaGenerationTest extends FunSuite {
  //val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/SQLSpecNoAnnotations.scala")
  val Directory = new File("generated")
  val file = new File("src/test/scala/de/tu_darmstadt/veritas/lemmagen/SQLSpecAnnotated.scala")
  val problem = new Problem(file)

  private def recursivedelete(file: File) {
    if (file.isDirectory)
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(recursivedelete(_))
    file.delete
  }

  if(Directory.exists())
    recursivedelete(Directory)

  val generator = new CleverHintsLemmaGenerator(problem) {
    override def makePipeline(constructor: GraphConstructor): LemmaGeneratorPipeline = {
      new VisualizingGeneratorPipeline(Directory, constructor, oracleConsultation, selectionHeuristic, postprocessor)
    }
  }

  def printRules(lemmas: Seq[Lemma]) = {
    val outputPrettyPrinter = new PrettyPrintWriter(new PrintWriter(System.out))
    val lemmaPrettyPrinter = new SimpleLaTeXLemmaPrinter {
      override val printer: PrettyPrintWriter = outputPrettyPrinter
    }
    lemmas.foreach { lemma =>
      lemmaPrettyPrinter.printTypingRule(lemma)
    }
    outputPrettyPrinter.flush()
  }

  println("generate progress ...")
  private val progressLemmas = generator.generateProgressLemmas()
  println("generate preservation ...")
  private val preservationLemmas = generator.generatePreservationLemmas()

  val lemmas: Seq[Lemma] = (progressLemmas.values.flatten ++ preservationLemmas.values.flatten).toSeq

  for(property <- problem.dsk.properties) {
    test(s"${property.name}") {
      val equivalentLemmas = lemmas.filter(entry => LemmaEquivalence.isEquivalent(property, entry))
      println("")
      println(s"Equivalent to ${property.name}: ${equivalentLemmas.size} out of ${lemmas.size}")
      assert(equivalentLemmas.nonEmpty)
    }
  }

    test("write updated megaspec") {
    println(s"progress lemmas: ${progressLemmas.values.flatten.size}")
    println(s"preservation lemmas: ${preservationLemmas.values.flatten.size}")
    println("")
    val specString = scala.io.Source.fromFile(file).mkString("")
    val input = Input.VirtualFile(file.getAbsolutePath, specString)
    val updatedString = ScalaSPLSpecificationOutput.addLemmasToSpecification(input, progressLemmas, preservationLemmas)
    val updatedFile = new File("generated/SQLSpecUpdated.scala")
    val writer = new BufferedWriter(new FileWriter(updatedFile))
    writer.write(updatedString)
    writer.close()
    succeed
  }
}
