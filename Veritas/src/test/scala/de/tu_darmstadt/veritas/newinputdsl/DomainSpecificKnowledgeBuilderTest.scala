package de.tu_darmstadt.veritas.newinputdsl

import java.io.File

import org.scalatest.FunSuite

class DomainSpecificKnowledgeBuilderTest extends FunSuite {
  val filesDir = new File("src/test/scala/de/tu_darmstadt/veritas/newinputdsl/dskbuilderfiles")
  test("Property Attached correctly gathered") {
    val builder = DomainSpecificKnowledgeBuilder()
    val file = new File(filesDir, "DSKProperty.scala")

    val dsk = builder.build(file)
    assert(dsk.attachedProperties.toSeq.head._1._1.signature.name == "plus")
    assert(dsk.attachedProperties.toSeq.head._1._2 == "falseProperty")

    assert(dsk.attachedProperties.toSeq(1)._1._1.signature.name == "predpred")
    assert(dsk.attachedProperties.toSeq(1)._1._2 == "trueProperty")

    assert(dsk.attachedProperties.toSeq(2)._1._1.signature.name == "plus")
    assert(dsk.attachedProperties.toSeq(2)._1._2 == "trueProperty")
  }

  test("Property Attached reference should fail") {
    val builder = DomainSpecificKnowledgeBuilder()
    val file = new File(filesDir, "DSKPropertyReferenceFail.scala")

    assertThrows[SPLTranslationError] {
      builder.build(file)
    }
  }

  test("Property Attached reference is not ensuring function") {
    val builder = DomainSpecificKnowledgeBuilder()
    val file = new File(filesDir, "DSKPropertyReferencedNotEnsuring.scala")

    assertThrows[SPLTranslationError] {
      builder.build(file)
    }
  }

  test("Recursive correctly gathered") {
    val builder = DomainSpecificKnowledgeBuilder()
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
    val builder = DomainSpecificKnowledgeBuilder()
    val file = new File(filesDir, "DSKRecursiveInnerHasMoreThanOneCtor.scala")

    assertThrows[SPLTranslationError] {
      builder.build(file)
    }
  }

  test("Recursive has at least one position given") {
    val builder = DomainSpecificKnowledgeBuilder()
    val file = new File(filesDir, "DSKRecursiveAtLeastOnePositionGiven.scala")

    assertThrows[SPLTranslationError] {
      builder.build(file)
    }
  }
}
