package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.evaluation

import java.io.{File, FileWriter}

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Problem
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.util.SimpleLaTeXLemmaPrinter
import de.tu_darmstadt.veritas.backend.ast.TypingRule
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter

/** Write general information about the case study to the directory `directory`. */
class GeneralInformation(problem: Problem, directory: File) extends EvaluationHelpers {
  ensureEmpty(directory)

  def writeBaselineLemmas(file: File): Unit = {
    val writer = new FileWriter(file)
    val latexWriter = new SimpleLaTeXLemmaPrinter {
      override val printer: PrettyPrintWriter = new PrettyPrintWriter(writer)
    }
    for (lemma <- problem.dsk.properties) {
      problem.enquirer.getAllVarTypes(lemma) // to force correct types
      writer.write(s"\\def\\lemmaBaseline${lemma.name}/{")
      latexWriter.printTypingRule(lemma)
      writer.write("}\n")
      writer.flush()
    }
    writer.close()
  }

  def writeLemmaClasses(): Unit = {
    val progress = problem.dsk.progressProperties.valuesIterator.flatten.toSet
    val preservation = problem.dsk.preservationProperties.valuesIterator.flatten.toSet
    val other = problem.dsk.properties -- (progress ++ preservation)

    def names(s: Set[TypingRule]): String = s.toSeq.sortBy(_.name).map(l => s"\\cod{${l.name})").mkString(", ")

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

  def writePreservables(): Unit = {
    val names = sortFunctions(problem.dsk.preservables.toSeq).map(formatFunctionName).mkString(", ")
    printToFile(new File(directory, "preservables.tex"), names)
  }

  def writeSpec(): Unit = {
    val spec = scala.io.Source.fromFile(problem.specFile).mkString("")
    printToFile(new File(directory, "SQLSpecAnnotated.scala"), spec)
  }

  /** This writes:
    *   - lemma-classes.tex, which contains the classification of baseline lemmas
    *   - static-functions.tex, which contains all functions annotated with @Static
    *   - dynamic-functions.tex, which contains all functions annotated with @Dynamic
    *   - preservables.tex, which contains all functions annotated with @Preservable
    *   - baseline-lemmas.tex, which contains LaTeX commands for all baseline lemmas
    *   - SQLSpecAnnotated.scala, which contains the original ScalaSPL specification
    */
  def write(): Unit = {
    writeLemmaClasses()
    writeStaticFunctions()
    writeDynamicFunctions()
    writePreservables()
    writeBaselineLemmas(new File(directory, "baseline-lemmas.tex"))
    writeSpec()
  }
}

