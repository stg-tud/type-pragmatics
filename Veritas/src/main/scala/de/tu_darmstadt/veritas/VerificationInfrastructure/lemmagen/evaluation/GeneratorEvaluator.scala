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
    Seq(problem.dsk.progressProperties, problem.dsk.preservationProperties, problem.dsk.auxiliaryProperties).flatMap {
      problem.dsk.lookupByFunName(_, fn.signature.name)
    }.toSet
  }

  def getBaselineLemmaClass(name: String): String = {
    if(problem.dsk.progressProperties.values.flatten.exists(_.name == name))
      "progress"
    else if (problem.dsk.preservationProperties.values.flatten.exists(_.name == name))
      "preservation"
    else if (problem.dsk.auxiliaryProperties.values.flatten.exists(_.name == name))
      "?"
    else
      "(unknown)"
  }

  def progressLemmas(lemmas: Seq[Lemma]): Seq[Lemma] = {
    lemmas.filter(_.name.contains("Progress"))
  }

  def preservationLemmas(lemmas: Seq[Lemma]): Seq[Lemma] = {
    lemmas.filter(_.name.contains("Preservation"))
  }

  def predicatePreservationLemmas(lemmas: Seq[Lemma]): Seq[Lemma] = {
    preservationLemmas(lemmas).filter(_.name.contains("welltyped"))
  }

  def relationalPreservationLemmas(lemmas: Seq[Lemma]): Seq[Lemma] = {
    preservationLemmas(lemmas).filter(_.name.contains("sameLength"))
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
    val generatedPredicatePreservation = predicatePreservationLemmas(lemmas)
    val generatedRelationalPreservation = relationalPreservationLemmas(lemmas)
    require((generatedProgress ++ generatedPredicatePreservation ++ generatedRelationalPreservation).toSet == lemmas.toSet)
    //val equivalentProgress = equivalent intersect generatedProgress
    //val equivalentPreservation = equivalent intersect generatedPreservation
    s"""${formatFunctionName(function)} &
       | ${generatedProgress.size} &
       | ${generatedPredicatePreservation.size} &
       | ${generatedRelationalPreservation.size} &
       | ${lemmas.size} \\\\""".stripMargin
  }

  def evaluate(writeSpec: Boolean = false): Unit = {
    val result = lemmaStore.loadOrGenerate(generator)
    val countLines = new mutable.ListBuffer[String]()
    val successLemmas = new mutable.HashMap[String, Boolean]()
    val successLines = new mutable.ListBuffer[String]()
    var totalGeneratedProgress = 0
    var totalGeneratedPredicatePreservation = 0
    var totalGeneratedRelationalPreservation = 0
    sortFunctions((problem.dsk.dynamicFunctions ++ problem.dsk.staticFunctions).toSeq).foreach { function =>
      val lemmas = result.getOrElse(function, Seq())
      val name = function.signature.name
      println(s"evaluating $name...")

      // equivalent lemmas
      val expectedLemmas = lookupExpectedLemmas(function)
      val equivalentLemmas = lemmas.filter(generated =>
        expectedLemmas.exists(expected =>
          LemmaEquivalence.isEquivalent(expected, generated)
        )
      )
      if(problem.dsk.dynamicFunctions.contains(function) && function.signature.out.name != "Bool") {
        // all lemmas
        printLemmas(new File(outputDirectory, s"generated-$name.txt"), lemmas)
        printLemmas(new File(outputDirectory, s"equivalent-$name.txt"), equivalentLemmas)
        totalGeneratedProgress += progressLemmas(lemmas).size
        totalGeneratedPredicatePreservation += predicatePreservationLemmas(lemmas).size
        totalGeneratedRelationalPreservation += relationalPreservationLemmas(lemmas).size
        countLines += generateGeneratedLine(function, lemmas)
      }
      if(function.signature.name != "reduce") {
        val sortedExpectedLemmas = expectedLemmas.toSeq.sortBy(_.name)
        sortedExpectedLemmas.foreach { lemma =>
          val success = equivalentLemmas.exists(l => LemmaEquivalence.isEquivalent(lemma, l))
          successLemmas(lemma.name) = success
        }
      }
    }
    val baselineOrdering = successLemmas.keys.groupBy(getBaselineLemmaClass).mapValues(it => it.toSeq.sortBy(_.toLowerCase))
    baselineOrdering.keys.toSeq.sortBy(_.toLowerCase).reverse.foreach { cls =>
      baselineOrdering(cls).foreach { lemmaName =>
        successLines += s"\\cod{${lemmaName}} & ${getBaselineLemmaClass(lemmaName)} &"
        successLines += (if (successLemmas(lemmaName)) "\\checkmark" else "$\\times$") + "\\\\"
      }
    }
    countLines += "\\addlinespace"
    countLines += s"total & $totalGeneratedProgress " +
      s"& $totalGeneratedPredicatePreservation " +
      s"& $totalGeneratedRelationalPreservation " +
      s"& ${totalGeneratedProgress + totalGeneratedPredicatePreservation + totalGeneratedRelationalPreservation} \\\\"
    printToFile(new File(outputDirectory, "counts-table.tex"), countLines.mkString("\n"))
    printToFile(new File(outputDirectory, "success-table.tex"), successLines.mkString("\n"))
    if(writeSpec) {
      // write updated spec
      val specString = scala.io.Source.fromFile(problem.specFile).mkString("")
      val input = Input.VirtualFile(problem.specFile.getAbsolutePath, specString)
      val (progress, preservation) = lemmaMap(result)
      val updated = ScalaSPLSpecificationOutput.addLemmasToSpecification(input, progress, preservation)
      printToFile(new File(outputDirectory, "UpdatedSpec.scala"), updated)
      val onlyLemmas = ScalaSPLSpecificationOutput.generateLemmasString(input, progress, preservation)
      printToFile(new File(outputDirectory, "Lemmas.scala"), onlyLemmas)
    }
    /*writeDynamicFunctions()
    writeStaticFunctions()
    writeLemmaClasses()
    writeBaselineLemmas(new File(Directory, "baseline-lemmas.txt"))Ü*/
  }



}
