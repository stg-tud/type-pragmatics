package de.tu_darmstadt.veritas.newinputdsl

import java.io.File

import de.tu_darmstadt.veritas.backend.ast.function._
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

  test("GroupedDistinction correctly gathered") {
    val builder = DomainSpecificKnowledgeBuilder()
    val file = new File(filesDir, "DSKGroupedDistinction.scala")

    val groupings = builder.build(file).groupings
    val eq1 = FunctionEq("predpred", Seq(FunctionPatApp("zero", Seq())), FunctionExpApp("zero", Seq()))
    val eq2 = FunctionEq("predpred", Seq(FunctionPatApp("succ", Seq(FunctionPatApp("zero", Seq())))), FunctionExpApp("zero", Seq()))
    val eq3 = FunctionEq("predpred", Seq(FunctionPatApp("succ", Seq(FunctionPatApp("succ", Seq(FunctionPatVar("n")))))), FunctionExpVar("n"))

    assert(groupings(0)(0) == eq1)
    assert(groupings(1)(0) == eq2)
    assert(groupings(1)(1) == eq3)
    assert(groupings(2)(0) == eq1)
    assert(groupings(2)(1) == eq3)
    assert(groupings(3)(0) == eq2)
    assert(groupings(4)(0) == eq3)
    assert(groupings(5)(0) == eq1)
  }

  test("GroupedDistinction has no positions") {
    val builder = DomainSpecificKnowledgeBuilder()
    val file = new File(filesDir, "DSKGroupedDistinctionEmpty.scala")

    assertThrows[SPLTranslationError] {
      builder.build(file)
    }
  }
}
