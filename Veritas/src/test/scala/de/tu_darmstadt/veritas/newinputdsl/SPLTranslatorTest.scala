package de.tu_darmstadt.veritas.newinputdsl

import java.io.File

import de.tu_darmstadt.veritas.backend.ast.{DataType, DataTypeConstructor, SortRef}
import org.scalatest.FunSuite

class SPLTranslatorTest extends FunSuite {
  val filesDir =
    new File("src/test/scala/de/tu_darmstadt/veritas/newinputdsl")
  test("translate simple adts") {
    val translator = new SPLTranslator
    val file = new File(filesDir, "ADTCorrect.scala")
    val module = translator.translate(file)
    println(module)
    assert(module.isSuccess)
    assert(module.get.name == "ADTCorrect")
    assert(module.get.defs.size == 3)
    assert(module.get.defs.head == DataType(true, "First", Seq()))
    assert(module.get.defs(1) == DataType(false, "Num", Seq(
      DataTypeConstructor("zero", Seq()),
      DataTypeConstructor("succ", Seq(SortRef("Num"))),
      DataTypeConstructor("succ2", Seq(SortRef("Num"), SortRef("First"))))))
    assert(module.get.defs(2) == DataType(true, "OtherNum", Seq(
      DataTypeConstructor("otherzero", Seq()),
      DataTypeConstructor("othersucc", Seq(SortRef("Num"))),
      DataTypeConstructor("othersucc2", Seq(SortRef("OtherNum"), SortRef("First"))))))
  }

  test("fail because trait has type parameter") {
    val translator = new SPLTranslator
    val file = new File(filesDir, "ADTFailTypeParams.scala")
    val module = translator.translate(file)
    assert(module.isFailure)
  }

  test("fail because case class has type parameter") {
    val translator = new SPLTranslator
    val file = new File(filesDir, "ADTFailCaseClassTypeParams.scala")
    val module = translator.translate(file)
    assert(module.isFailure)
  }

  test("fail because case class has no own defined base trait") {
    val translator = new SPLTranslator
    val file = new File(filesDir, "ADTFailCaseClassNoBaseTrait.scala")
    val module = translator.translate(file)
    assert(module.isFailure)
  }

  test("fail because case class inherits from Expression") {
    val translator = new SPLTranslator
    val file = new File(filesDir, "ADTFailCaseClassExpressionBase.scala")
    val module = translator.translate(file)
    assert(module.isFailure)
  }

}
