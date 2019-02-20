package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import java.io.StringWriter

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.scalaspl.prettyprint.SimpleToScalaSPLSpecificationPrinter
import scalafix.internal.patch.PatchInternals

import scala.meta._
import scalafix.v1._

class ScalaSPLSpecificationOutput(input: Input,
                                  lemmas: Seq[Lemma],
                                  annotationsToAdd: Seq[(String, String, String)]) {

  def this(fromString: String, lemmas: Seq[Lemma], annotationsToAdd: Seq[(String, String, String)]) = {
    this(Input.VirtualFile("test.scala", fromString), lemmas, annotationsToAdd)
  }

  def addDSKAnnotation(doc: SyntacticDocument,
                       functionName: String,
                       annotationName: String,
                       lemmaName: String): Patch = {
    doc.tree.collect {
      case fn@Defn.Def(_, Term.Name(foundName), _, _, _, _) if foundName == functionName =>
        // we have found ``functionName``, we add an annotation
        val lemmaString = "\"" + lemmaName + "\""
        val annotation = s"@$annotationName($lemmaString)"
        Patch.addLeft(fn, annotation + "\n  ")
    }.asPatch
  }

  def makeLemmasString(): String = {
    val writer = new StringWriter()
    val prettyWriter = new PrettyPrintWriter(writer)
    prettyWriter.indent() // we use a default indentation of 2 spaces
    val lemmaWriter = new SimpleToScalaSPLSpecificationPrinter {
      override val printer: PrettyPrintWriter = prettyWriter
    }
    for(lemma <- lemmas) {
      prettyWriter.write("// The following lemmas have been automatically generated")
      prettyWriter.newline()
      prettyWriter.write("@Property")
      prettyWriter.newline()
      lemmaWriter.printTypingRule(lemma)
      prettyWriter.flush()
    }
    writer.toString
  }

  def addLemmas(doc: SyntacticDocument): Patch = {
    val lemmasString = makeLemmasString()
    doc.tree.collect {
      case obj@Defn.Object(_, _, Template(_, _, self, stats)) =>
        // we have found the top-level object, add the lemma
        Patch.addLeft(obj.tokens.last, lemmasString)
    }.asPatch
  }

  def generate(): String = {
    val doc = SyntacticDocument.fromInput(input)
    var dskPatch = Patch.empty
    for((functionName, annotationName, lemmaName) <- annotationsToAdd) {
      dskPatch += addDSKAnnotation(doc, functionName, annotationName, lemmaName)
    }
    val lemmaPatch = addLemmas(doc)
    PatchInternals.syntactic(
      Map(RuleName("AddDSKAnnotations") -> dskPatch, RuleName("AddLemmas") -> lemmaPatch),
      doc, suppress = true)._1
  }
}

object ScalaSPLSpecificationOutput {
  private def flattenLemmas(allLemmas: Map[FunctionDef, Seq[Lemma]],
                            annotation: String): Seq[(String, String, String)] = {
    allLemmas.flatMap {
      case (fn, lemmas) => lemmas.map(lemma => (fn.signature.name, annotation, lemma.name))
    }.toSeq
  }

  def addLemmasToSpecification(input: Input,
                               progressLemmas: Map[FunctionDef, Seq[Lemma]],
                               preservationLemmas: Map[FunctionDef, Seq[Lemma]]): String = {
    val annotations = (flattenLemmas(progressLemmas, "ProgressProperty")
      ++ flattenLemmas(preservationLemmas, "PreservationProperty"))
    val lemmas = (progressLemmas ++ preservationLemmas).flatMap(_._2).toSeq
    val writer = new ScalaSPLSpecificationOutput(input, lemmas, annotations)
    writer.generate()
  }

  def addLemmasToSpecification(input: String,
                               progressLemmas: Map[FunctionDef, Seq[Lemma]],
                               preservationLemmas: Map[FunctionDef, Seq[Lemma]]): String = {
    addLemmasToSpecification(Input.VirtualFile("placeholder.scala", input), progressLemmas, preservationLemmas)
  }
}