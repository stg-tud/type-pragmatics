package de.tu_darmstadt.veritas.newinputdsl

import java.io.File

import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast.{DataType, DataTypeConstructor, Functions, SortRef}
import org.scalatest.FunSuite

class SPLTranslatorTest extends FunSuite {
  val filesDir =
    new File("src/test/scala/de/tu_darmstadt/veritas/newinputdsl")

  test("translate simple adts") {
    val translator = new SPLTranslator
    val file = new File(filesDir, "ADTCorrect.scala")
    val module = translator.translate(file)
    println(module)
    assert(module.name == "ADTCorrect")
    assert(module.defs.size == 4)
    assert(module.defs.head == DataType(true, "First", Seq()))
    assert(module.defs(1) == DataType(false, "Num", Seq(
      DataTypeConstructor("zero", Seq()),
      DataTypeConstructor("succ", Seq(SortRef("Num"))),
      DataTypeConstructor("succ2", Seq(SortRef("Num"), SortRef("First"))))))
    assert(module.defs(2) == DataType(true, "OtherNum", Seq(
      DataTypeConstructor("otherzero", Seq()),
      DataTypeConstructor("othersucc", Seq(SortRef("Num"))),
      DataTypeConstructor("othersucc2", Seq(SortRef("OtherNum"), SortRef("First"))))))
  }

  test("fail because trait has type parameter") {
    val translator = new SPLTranslator
    val file = new File(filesDir, "ADTFailTypeParams.scala")
    assertThrows[IllegalArgumentException] {
      translator.translate(file)
    }
  }

  test("fail because case class has type parameter") {
    val translator = new SPLTranslator
    val file = new File(filesDir, "ADTFailCaseClassTypeParams.scala")
    assertThrows[IllegalArgumentException] {
      translator.translate(file)
    }
  }

  test("fail because case class has no own defined base trait") {
    val translator = new SPLTranslator
    val file = new File(filesDir, "ADTFailCaseClassNoBaseTrait.scala")
    assertThrows[IllegalArgumentException] {
      translator.translate(file)
    }
  }

  test("fail because case class inherits from Expression") {
    val translator = new SPLTranslator
    val file = new File(filesDir, "ADTFailCaseClassExpressionBase.scala")
    assertThrows[IllegalArgumentException] {
      translator.translate(file)
    }
  }

  test("translate functions correctly") {
    val translator = new SPLTranslator
    val file = new File(filesDir, "FunctionCorrect.scala")
    val module = translator.translate(file)
    val fns = module.defs.collect { case f: Functions => f}.head
    val zeroExp = FunctionExpApp("zero", Seq())
    val zeroPat = FunctionPatApp("zero", Seq())
    assert(fns.funcs(0) ==
      FunctionDef(
        FunctionSig("pred", Seq(SortRef("Num")), SortRef("Num")),
        Seq(
          FunctionEq("pred", Seq(zeroPat), zeroExp),
          FunctionEq("pred", Seq(FunctionPatApp("succ", Seq(FunctionPatVar("n")))), FunctionExpVar("n"))
        )))
    assert(fns.funcs(1) ==
      FunctionDef(
        FunctionSig("predpred", Seq(SortRef("Num")), SortRef("Num")),
        Seq(
          FunctionEq("predpred", Seq(zeroPat), zeroExp),
          FunctionEq("predpred", Seq(FunctionPatApp("succ", Seq(zeroPat))), zeroExp),
          FunctionEq("predpred", Seq(FunctionPatApp("succ", Seq(FunctionPatApp("succ", Seq(FunctionPatVar("n")))))), FunctionExpVar("n"))
        )))
    assert(fns.funcs(2) ==
      FunctionDef(
        FunctionSig("plus", Seq(SortRef("Num"), SortRef("Num")), SortRef("Num")),
        Seq(
          FunctionEq("plus", Seq(zeroPat, FunctionPatVar("b")),
            FunctionExpIf(
              FunctionExpOr(
                FunctionExpAnd(
                  FunctionExpEq(FunctionExpVar("b"), zeroExp),
                  FunctionExpEq(FunctionExpVar("b"), FunctionExpVar("b"))),
                  FunctionExpNeq(FunctionExpVar("a"), zeroExp)),
              zeroExp,
              FunctionExpApp("succ", Seq(FunctionExpVar("b")))
            )),
          FunctionEq("plus",
            Seq(
              FunctionPatVar("a"),
              FunctionPatApp("succ", Seq(FunctionPatVar("n")))),
            FunctionExpApp("succ",
              Seq(
                FunctionExpApp("plus",
                  Seq(FunctionExpVar("a"), FunctionExpVar("n")))))
          ))))
    assert(fns.funcs(3) ==
      FunctionDef(
        FunctionSig("singlelet", Seq(SortRef("Num"), SortRef("YN")), SortRef("YN")),
        Seq(
          FunctionEq("singlelet", Seq(zeroPat, FunctionPatApp("yes", Seq())),
            FunctionExpLet("x",
              FunctionExpApp("plus", Seq(zeroExp, FunctionExpApp("succ", Seq(zeroExp)))),
              FunctionExpIf(
                FunctionExpNot(
                  FunctionExpEq(FunctionExpVar("x"), zeroExp)),
                  FunctionExpApp("yes", Seq()),
                  FunctionExpApp("no", Seq())))))))
    assert(fns.funcs(4) ==
      FunctionDef(
        FunctionSig("multiplelets", Seq(SortRef("Num"), SortRef("YN")), SortRef("YN")),
        Seq(
          FunctionEq("multiplelets", Seq(zeroPat, FunctionPatApp("yes", Seq())),
            FunctionExpLet("x",
              FunctionExpApp("plus", Seq(zeroExp, FunctionExpApp("succ", Seq(zeroExp)))),
              FunctionExpLet("y",
                FunctionExpApp("plus", Seq(zeroExp, FunctionExpApp("succ", Seq(zeroExp)))),
                FunctionExpLet("z",
                  FunctionExpApp("plus", Seq(zeroExp, FunctionExpApp("succ", Seq(zeroExp)))),
                FunctionExpIf(
                  FunctionExpBiImpl(
                    FunctionExpEq(FunctionExpVar("x"), zeroExp),
                    FunctionExpEq(FunctionExpVar("y"), zeroExp)),
                  FunctionExpApp("yes", Seq()),
                  FunctionExpApp("no", Seq())))))))))
  }

  test("fail because function definied within function") {
    val translator = new SPLTranslator
    val file = new File(filesDir, "FunctionFailDefFunction.scala")
    assertThrows[IllegalArgumentException] {
      translator.translate(file)
    }
  }

  test("fail because function has type params") {
    val translator = new SPLTranslator
    val file = new File(filesDir, "FunctionFailTypeParams.scala")
    assertThrows[IllegalArgumentException] {
      translator.translate(file)
    }
  }

  test("fail because top of function not match") {
    val translator = new SPLTranslator
    val file = new File(filesDir, "FunctionFailTopNotMatch.scala")
    assertThrows[IllegalArgumentException] {
      translator.translate(file)
    }
  }
}
