package de.tu_darmstadt.veritas.scalaspl

import java.io.File

import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.scalaspl.dsk.VeritasDomainSpecificKnowledgeBuilder
import de.tu_darmstadt.veritas.scalaspl.translator.ScalaSPLTranslationError
import org.scalatest.FunSuite

class VeritasDomainSpecificKnowledgeBuilderTest extends FunSuite {
  val filesDir = new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/dskbuilderfiles")

  test("Recursive correctly gathered") {
    val builder = VeritasDomainSpecificKnowledgeBuilder()
    val file = new File(filesDir, "DSKRecursive.scala")
    val dsk = builder.build(file)

    assert(dsk.recursiveFunctions.toSeq.head._1.signature.name == "twoParams")
    assert(dsk.recursiveFunctions.toSeq.head._2.name == "inner")

    assert(dsk.recursiveFunctions.toSeq(1)._1.signature.name == "recursiveTwoLevel")
    assert(dsk.recursiveFunctions.toSeq(1)._2.name == "inner")

    assert(dsk.recursiveFunctions.toSeq(2)._1.signature.name == "recursiveOneLevel")
    assert(dsk.recursiveFunctions.toSeq(2)._2.name == "inner")
  }

  test("Recursive inner adt has two ctors") {
    val builder = VeritasDomainSpecificKnowledgeBuilder()
    val file = new File(filesDir, "DSKRecursiveInnerHasMoreThanOneCtor.scala")

    assertThrows[ScalaSPLTranslationError] {
      builder.build(file)
    }
  }

  test("Recursive has at least one position given") {
    val builder = VeritasDomainSpecificKnowledgeBuilder()
    val file = new File(filesDir, "DSKRecursiveAtLeastOnePositionGiven.scala")

    assertThrows[ScalaSPLTranslationError] {
      builder.build(file)
    }
  }

  test("QLSpec dsk") {
    val builder = VeritasDomainSpecificKnowledgeBuilder()
    val file = new File(filesDir, "../QLSpec.scala")
    val dsk = builder.build(file)
    println(dsk.failableTypes)
    println(dsk.progressProperties)
    println(dsk.preservationProperties)
    println(dsk.recursiveFunctions)
  }
}
