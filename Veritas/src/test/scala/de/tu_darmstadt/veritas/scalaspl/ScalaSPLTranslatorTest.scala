package de.tu_darmstadt.veritas.scalaspl

import java.io.File

import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.scalaspl.translator.{ScalaSPLTranslationError, ScalaSPLTranslator}
import org.scalatest.FunSuite

class ScalaSPLTranslatorTest extends FunSuite {
  val filesDir =
    new File("src/test/scala/de/tu_darmstadt/veritas/scalaspl/translatorfiles")

  test("translate simple adts") {
    val translator = new ScalaSPLTranslator
    val file = new File(filesDir, "ADTCorrect.scala")
    val module = translator.translate(file)
    assert(module.name == "ADTCorrect")
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
    val translator = new ScalaSPLTranslator
    val file = new File(filesDir, "ADTFailTypeParams.scala")
    assertThrows[ScalaSPLTranslationError] {
      translator.translate(file)
    }
  }

  test("fail because case class has type parameter") {
    val translator = new ScalaSPLTranslator
    val file = new File(filesDir, "ADTFailCaseClassTypeParams.scala")
    assertThrows[ScalaSPLTranslationError] {
      translator.translate(file)
    }
  }

  test("fail because case class has no own defined base trait") {
    val translator = new ScalaSPLTranslator
    val file = new File(filesDir, "ADTFailCaseClassNoBaseTrait.scala")
    assertThrows[ScalaSPLTranslationError] {
      translator.translate(file)
    }
  }

  test("fail because case class inherits from Expression") {
    val translator = new ScalaSPLTranslator
    val file = new File(filesDir, "ADTFailCaseClassExpressionBase.scala")
    assertThrows[ScalaSPLTranslationError] {
      translator.translate(file)
    }
  }

  val zeroExp = FunctionExpApp("zero", Seq())
  test("translate functions correctly") {
    val translator = new ScalaSPLTranslator
    val file = new File(filesDir, "FunctionCorrect.scala")
    val module = translator.translate(file)
    val fns = module.defs.collect { case f: Functions => f}.head
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
        FunctionSig("wildcard", Seq(SortRef("Num"), SortRef("Num")), SortRef("Num")),
        Seq(
          FunctionEq("wildcard", Seq(zeroPat, FunctionPatVar("wildcardName0")),
            FunctionExpIf(
              FunctionExpOr(
                FunctionExpAnd(
                  FunctionExpEq(FunctionExpVar("b"), zeroExp),
                  FunctionExpEq(FunctionExpVar("b"), FunctionExpVar("b"))),
                FunctionExpNeq(FunctionExpVar("a"), zeroExp)),
              zeroExp,
              FunctionExpApp("succ", Seq(FunctionExpVar("b")))
            )),
          FunctionEq("wildcard",
            Seq(
              FunctionPatVar("wildcardName0"),
              FunctionPatApp("succ", Seq(FunctionPatVar("wildcardName1")))),
            zeroExp))))
    assert(fns.funcs(4) ==
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
    assert(fns.funcs(5) ==
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
    val translator = new ScalaSPLTranslator
    val file = new File(filesDir, "FunctionFailDefFunction.scala")
    assertThrows[ScalaSPLTranslationError] {
      translator.translate(file)
    }
  }

  test("fail because function has type params") {
    val translator = new ScalaSPLTranslator
    val file = new File(filesDir, "FunctionFailTypeParams.scala")
    assertThrows[ScalaSPLTranslationError] {
      translator.translate(file)
    }
  }

  test("fail because top of function not match") {
    val translator = new ScalaSPLTranslator
    val file = new File(filesDir, "FunctionFailTopNotMatch.scala")
    assertThrows[ScalaSPLTranslationError] {
      translator.translate(file)
    }
  }

  test("translate typingrules correctly") {
    val translator = new ScalaSPLTranslator
    val file = new File(filesDir, "AxiomCorrect.scala")
    val module = translator.translate(file)
    val axioms = module.defs.collect { case a: Axioms => a}
    val lemmas = module.defs.collect { case a: Lemmas => a}
    val goals = module.defs.collect { case a: Goals => a}
    assert(axioms(0).axioms(0) ==
      TypingRule("simple",
        Seq(
          FunctionExpJudgment(FunctionExpFalse),
          FunctionExpJudgment(FunctionExpTrue)),
        Seq(
          ForallJudgment(
          Seq(MetaVar("x")),
          Seq(FunctionExpJudgment(
            FunctionExpEq(
              FunctionExpApp("succ", Seq(FunctionMeta(MetaVar("x")))),
              FunctionExpApp("zero", Seq()))))),
            FunctionExpJudgment(FunctionExpTrue))))

    assert(axioms(0).axioms(1) ==
      TypingRule("orcase", Seq(),
        Seq(OrJudgment(Seq(
          Seq(
            FunctionExpJudgment(FunctionExpEq(zeroExp, zeroExp)),
            FunctionExpJudgment(FunctionExpEq(zeroExp, FunctionExpApp("succ", Seq(zeroExp))))),
          Seq(FunctionExpJudgment(FunctionExpEq(zeroExp, zeroExp))))))))

    assert(axioms(0).axioms(2) ==
      TypingRule("multipleconcls", Seq(),
        Seq(
          FunctionExpJudgment(FunctionExpEq(zeroExp, zeroExp)),
          FunctionExpJudgment(FunctionExpEq(zeroExp, FunctionExpApp("succ", Seq(zeroExp)))),
          FunctionExpJudgment(FunctionExpTrue))))

    assert(lemmas(0).lemmas(0) ==
      TypingRule("metavariables",
        Seq(
          FunctionExpJudgment(FunctionExpTrue),
          ExistsJudgment(
            Seq(MetaVar("x"), MetaVar("y")),
            Seq(
              FunctionExpJudgment(FunctionExpNeq(FunctionMeta(MetaVar("x")), FunctionMeta(MetaVar("y")))),
              FunctionExpJudgment(FunctionExpEq(FunctionMeta(MetaVar("a")), FunctionMeta(MetaVar("x"))))))),
        Seq(
          ForallJudgment(
            Seq(MetaVar("x")),
            Seq(FunctionExpJudgment(
              FunctionExpEq(
                FunctionExpApp("succ", Seq(FunctionMeta(MetaVar("x")))),
                FunctionMeta(MetaVar("z")))))))))

    assert(goals(0).goals(0) ==
      TypingRule("typing",
        Seq(
          TypingJudgmentSimple(FunctionMeta(MetaVar("z")), FunctionExpApp("atyp", Seq())),
          TypingJudgment(FunctionExpApp("cempty", Seq()), FunctionMeta(MetaVar("a")), FunctionExpApp("atyp", Seq()))),
        Seq(FunctionExpJudgment(FunctionExpTrue))))
  }

  test("local block is correctly translated") {
    val translator = new ScalaSPLTranslator
    val file = new File(filesDir, "LocalCorrect.scala")
    val module = translator.translate(file)
    val locals = module.defs.collect { case l: Local => l }
    assert(locals.head ==
      Local(
        Seq(
          PartialFunctions(Seq()),
          Functions(Seq()),
          Axioms(Seq( TypingRule("non", Seq(), Seq(FunctionExpJudgment(FunctionExpTrue))))),
          Lemmas(Seq(), None),
          Goals(Seq(), None),
          Consts(Seq(ConstDecl("a", SortRef("Num")), ConstDecl("b", SortRef("Num"))), false),
          Consts(Seq(ConstDecl("c", SortRef("Num")), ConstDecl("d", SortRef("Num"))), true),
          Consts(Seq(ConstDecl("e", SortRef("Num")), ConstDecl("f", SortRef("Num"))), false))))
  }
}
