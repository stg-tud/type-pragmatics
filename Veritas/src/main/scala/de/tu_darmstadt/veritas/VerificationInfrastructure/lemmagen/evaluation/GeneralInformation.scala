package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.evaluation

import java.io.{File, FileWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Problem
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.SimpleLemmaPrinter
import de.tu_darmstadt.veritas.backend.ast.TypingRule
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

class GeneralInformation(problem: Problem, directory: File) extends EvaluationHelpers {
  ensureEmpty(directory)

  def writeBaselineLemmas(file: File): Unit = {
    val writer = new FileWriter(file)
    val latexWriter = new SimpleLemmaPrinter {
      override val printer: PrettyPrintWriter = new PrettyPrintWriter(writer)
    }
    for (lemma <- problem.dsk.properties) {
      problem.enquirer.getAllVarTypes(lemma) // to force correct types
      writer.write(lemma.name + "\n")
      writer.write("--------------\n")
      latexWriter.printTypingRule(lemma)
      writer.write("\n")
      writer.flush()
    }
    writer.close()
  }

  def writeLemmaClasses(): Unit = {
    val progress = problem.dsk.progressProperties.valuesIterator.flatten.toSet
    val preservation = problem.dsk.preservationProperties.valuesIterator.flatten.toSet
    val other = problem.dsk.properties -- (progress ++ preservation)

    def names(s: Set[TypingRule]): String = s.toSeq.sortBy(_.name).map(l => s"\\C{${l.name})").mkString(", ")

    printToFile(new File(directory, "lemma-classes.tex"),
      s"""
         | progress lemma & ${names(progress)} \\\\
         | preservation lemma & ${names(preservation)} \\\\
         | other lemmas & ${names(other)}
       """.stripMargin)
  }

  def writeStaticFunctions(): Unit = {
    val names = sortFunctions(problem.dsk.staticFunctions.toSeq).map(formatFunctionName).mkString(", ")
    printToFile(new File(directory, "static-functions.tex"), names)
  }

  def writeDynamicFunctions(): Unit = {
    val names = sortFunctions(problem.dsk.dynamicFunctions.toSeq).map(formatFunctionName).mkString(", ")
    printToFile(new File(directory, "dynamic-functions.tex"), names)
  }

  def write(): Unit = {
    writeLemmaClasses()
    writeStaticFunctions()
    writeDynamicFunctions()
    writeBaselineLemmas(new File(directory, "baseline-lemmas.tex"))
  }
}

