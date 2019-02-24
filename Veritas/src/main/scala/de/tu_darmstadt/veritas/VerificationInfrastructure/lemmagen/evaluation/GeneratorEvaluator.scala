package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.evaluation

import java.io.{File, FileWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.ScalaSPLSpecificationOutput
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, LemmaEquivalence, LemmaGenerator, Problem}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.SimpleLemmaPrinter
import de.tu_darmstadt.veritas.backend.ast.TypingRule
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.prettyprint.SimpleToScalaSPLSpecificationPrinter

import scala.collection.mutable
import scala.meta.inputs.Input

class GeneratorEvaluator(problem: Problem,
                         generator: LemmaGenerator,
                         lemmaStoreFile: File,
                         outputDirectory: File) extends EvaluationHelpers {
  ensureEmpty(outputDirectory)

  val lemmaStore = new LemmaStore(lemmaStoreFile)

  def printLemmas(file: File, lemmas: Seq[Lemma]): Unit = {
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
      latexWriter.printTypingRule(lemma)
      writer.write("\n")
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

  def progressLemmas(lemmas: Seq[Lemma]): Seq[Lemma] = {
    lemmas.filter(_.name.contains("Progress"))
  }

  def preservationLemmas(lemmas: Seq[Lemma]): Seq[Lemma] = {
    lemmas.filter(_.name.contains("Preservation"))
  }

  def lemmaMap(lemmas: Map[FunctionDef, Seq[Lemma]]): (Map[FunctionDef, Seq[Lemma]], Map[FunctionDef, Seq[Lemma]]) = {
    val progress = lemmas.map {
      case (fn, lst) => (fn, progressLemmas(lst))
    }
    val preservation = lemmas.map {
      case (fn, lst) => (fn, preservationLemmas(lst))
    }
    (progress, preservation)
  }

  def generateGeneratedLine(function: FunctionDef, lemmas: Seq[Lemma]): String = {
    val generatedProgress = progressLemmas(lemmas)
    val generatedPreservation = preservationLemmas(lemmas)
    require((generatedProgress ++ generatedPreservation).toSet == lemmas.toSet)
    //val equivalentProgress = equivalent intersect generatedProgress
    //val equivalentPreservation = equivalent intersect generatedPreservation
    s"""${formatFunctionName(function)} &
       | ${generatedProgress.size} &
       | ${generatedPreservation.size} &
       | ${lemmas.size} \\\\""".stripMargin
  }

  def generateEquivalentLine(function: FunctionDef, lemmas: Seq[Lemma], equivalent: Seq[Lemma]): String = {
    val generatedProgress = progressLemmas(lemmas)
    val generatedPreservation = preservationLemmas(lemmas)
    val expectedProgress = problem.dsk.lookupByFunName(
      problem.dsk.progressProperties,
      function.signature.name).toSet
    val expectedPreservation = problem.dsk.lookupByFunName(
      problem.dsk.preservationProperties,
      function.signature.name).toSet
    require((generatedProgress ++ generatedPreservation).toSet == lemmas.toSet)
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

  def evaluate(writeSpec: Boolean = false): Unit = {
    val result = lemmaStore.loadOrGenerate(generator)
    val countLines = new mutable.ListBuffer[String]()
    val equivalentLines = new mutable.ListBuffer[String]()
    val successLines = new mutable.ListBuffer[String]()
    sortFunctions(result.keys.toSeq).foreach { function =>
      val lemmas = result(function)
      val name = function.signature.name
      println(s"evaluating $name...")
      // all lemmas
      printLemmas(new File(outputDirectory, s"generated-$name.txt"), lemmas)
      // equivalent lemmas
      val expectedLemmas = lookupExpectedLemmas(function)
      val equivalentLemmas = lemmas.filter(generated =>
        expectedLemmas.exists(expected =>
          LemmaEquivalence.isEquivalent(expected, generated)
        )
      )
      printLemmas(new File(outputDirectory, s"equivalent-$name.txt"), equivalentLemmas)
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
    printToFile(new File(outputDirectory, "counts-table.tex"), countLines.mkString("\n"))
    printToFile(new File(outputDirectory, "equivalent-table.tex"), equivalentLines.mkString("\n"))
    printToFile(new File(outputDirectory, "success-table.tex"), successLines.mkString("\n"))
    if(writeSpec) {
      // write updated spec
      val specString = scala.io.Source.fromFile(problem.specFile).mkString("")
      val input = Input.VirtualFile(problem.specFile.getAbsolutePath, specString)
      val (progress, preservation) = lemmaMap(result)
      val updated = ScalaSPLSpecificationOutput.addLemmasToSpecification(input, progress, preservation)
      printToFile(new File(outputDirectory, "UpdatedSpec.scala"), updated)
    }
    /*writeDynamicFunctions()
    writeStaticFunctions()
    writeLemmaClasses()
    writeBaselineLemmas(new File(Directory, "baseline-lemmas.txt"))Ü*/
  }



}
