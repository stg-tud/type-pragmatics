package de.tu_darmstadt.veritas.lemmagen

import java.io._

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.naive.NaiveLemmaGenerator
import de.tu_darmstadt.veritas.backend.ast.TypingRule
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.prettyprint.SimpleToScalaSPLSpecificationPrinter

import scala.collection.mutable

object NaiveEvaluation {
  val LemmaFile = new File("lemmas-naive.dat")
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

  def printLemmas(file: File, lemmas: Set[Lemma]): Unit = {
    println(s"Writing ${lemmas.size} lemmas to $file ...")
    val writer = new FileWriter(file)
    val lemmaWriter = new SimpleToScalaSPLSpecificationPrinter {
      override val printer: PrettyPrintWriter = new PrettyPrintWriter(writer)
    }
    val latexWriter = new SimpleLemmaPrinter {
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
      writer.write("\n")
      latexWriter.printTypingRule(lemma)
      writer.write("\n")
      writer.flush()
    }
    writer.close()
  }

  def printToFile(file: File, content: String): Unit = {
    val writer = new FileWriter(file)
    writer.write(content)
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

  def generateGeneratedLine(function: FunctionDef, lemmas: Set[Lemma]): String = {
    val generatedProgress = lemmas.filter(_.name.endsWith("Progress"))
    val generatedPreservation = lemmas.filter(_.name.endsWith("Preservation"))
    require(generatedProgress ++ generatedPreservation == lemmas)
    //val equivalentProgress = equivalent intersect generatedProgress
    //val equivalentPreservation = equivalent intersect generatedPreservation
    s"""${formatFunctionName(function)} &
    | ${generatedProgress.size} &
    | ${generatedPreservation.size} &
    | ${lemmas.size} \\\\""".stripMargin
  }

  def generateEquivalentLine(function: FunctionDef, lemmas: Set[Lemma], equivalent: Set[Lemma]): String = {
    val generatedProgress = lemmas.filter(_.name.endsWith("Progress"))
    val generatedPreservation = lemmas.filter(_.name.endsWith("Preservation"))
    val expectedProgress = problem.dsk.lookupByFunName(
      problem.dsk.progressProperties,
      function.signature.name).toSet
    val expectedPreservation = problem.dsk.lookupByFunName(
      problem.dsk.preservationProperties,
      function.signature.name).toSet
    require(generatedProgress ++ generatedPreservation == lemmas)
    val equivalentProgress = equivalent intersect generatedProgress
    val equivalentPreservation = equivalent intersect generatedPreservation
    val Precision = "%1.4f"

    def percentage(a: Integer, b: Integer): String = {
      if(b == 0) {
        "-"
      } else {
        Precision.format(a.toDouble / b.toDouble)
      }
    }
    /*
     ${equivalentProgress.size + equivalentPreservation.size} & ${expectedProgress.size + expectedPreservation.size}
     ${percentage(equivalentProgress.size, expectedProgress.size)} &
 ${percentage(equivalentPreservation.size, expectedPreservation.size)} &
 ${percentage((equivalentProgress.size + equivalentPreservation.size), (expectedProgress.size + expectedPreservation.size))} &
     */
    /*
        s"""${formatFunctionName(function)} &
       | ${equivalentProgress.size} & ${expectedProgress.size} &
       | ${equivalentPreservation.size} & ${expectedPreservation.size} \\\\""".stripMargin
     */
    s"""${formatFunctionName(function)} &
       | ${equivalentProgress.size} / ${expectedProgress.size} &
       | ${equivalentPreservation.size} / ${expectedPreservation.size} \\\\""".stripMargin
  }

  def evaluate(result: Map[FunctionDef, Set[Lemma]]): Unit = {
    val countLines = new mutable.ListBuffer[String]()
    val equivalentLines = new mutable.ListBuffer[String]()
    val successLines = new mutable.ListBuffer[String]()
    sortFunctions(result.keys.toSeq).filterNot(_.signature.name == "reduce").foreach { function =>
      val lemmas = result(function)
      val name = function.signature.name
      println(s"evaluating $name...")
      // all lemmas
      printLemmas(new File(Directory, s"generated-$name.txt"), lemmas)
      // equivalent lemmas
      val expectedLemmas = lookupExpectedLemmas(function)
      val equivalentLemmas = lemmas.filter(generated =>
        expectedLemmas.exists(expected =>
          LemmaEquivalence.isEquivalent(expected, generated)
        )
      )
      printLemmas(new File(Directory, s"equivalent-$name.txt"), equivalentLemmas)
      countLines += generateGeneratedLine(function, lemmas)
      equivalentLines += generateEquivalentLine(function, lemmas, equivalentLemmas)

      val sortedExpectedLemmas = expectedLemmas.toSeq.sortBy(_.name)
      var firstIteration = true
      sortedExpectedLemmas.foreach { lemma =>
        if(firstIteration) {
          successLines += s"${formatFunctionName(function)}"
          firstIteration = false
        }
        successLines += "&"
        successLines += s"\\C{${lemma.name}} &"
        val success = equivalentLemmas.exists(l => LemmaEquivalence.isEquivalent(lemma, l))
        successLines += (if(success) "\\checkmark" else "$\\times$") + "\\\\"
      }
    }
    printToFile(new File(Directory, "naive-counts-table.tex"), countLines.mkString("\n"))
    printToFile(new File(Directory, "naive-equivalent-table.tex"), equivalentLines.mkString("\n"))
    printToFile(new File(Directory, "naive-success-table.tex"), successLines.mkString("\n"))
    writeDynamicFunctions()
    writeStaticFunctions()
    writeLemmaClasses()
    writeBaselineLemmas(new File(Directory, "baseline-lemmas.txt"))
  }

  def writeBaselineLemmas(file: File): Unit = {
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
  def serializeLemmas(file: File, lemmas: Map[FunctionDef, Set[Lemma]]): Unit = {
    val stream = new ObjectOutputStream(new FileOutputStream(file))
    stream.writeObject(lemmas)
    stream.close()
  }

  def deserializeLemmas(file: File): Map[FunctionDef, Set[Lemma]] = {
    val stream = new ObjectInputStream(new FileInputStream(file))
    val lemmas = stream.readObject().asInstanceOf[Map[FunctionDef, Set[Lemma]]]
    stream.close()
    lemmas
  }

  def sortFunctions(functions: Seq[FunctionDef]): Seq[FunctionDef] = {
    functions.sortBy(_.signature.name)
  }

  def writeStaticFunctions(): Unit = {
    val names = sortFunctions(problem.dsk.staticFunctions.toSeq).map(formatFunctionName).mkString(", ")
    printToFile(new File(Directory, "static-functions.tex"), names)
  }

  def writeDynamicFunctions(): Unit = {
    val names = sortFunctions(problem.dsk.dynamicFunctions.toSeq).map(formatFunctionName).mkString(", ")
    printToFile(new File(Directory, "dynamic-functions.tex"), names)
  }

  def formatFunctionName(f: FunctionDef): String = {
    s"\\C{${f.signature.name}}"
  }

  def main(args: Array[String]): Unit = {
    var lemmas = Map.empty[FunctionDef, Set[Lemma]]
    if(!LemmaFile.exists()) {
      println("generating lemmas ...")
      lemmas = generate()
      println(s"serializing lemmas to $LemmaFile ...")
      serializeLemmas(LemmaFile, lemmas)
    } else {
      println(s"deserializing lemmas from $LemmaFile ...")
      lemmas = deserializeLemmas(LemmaFile)
    }
    evaluate(lemmas)
  }
}
