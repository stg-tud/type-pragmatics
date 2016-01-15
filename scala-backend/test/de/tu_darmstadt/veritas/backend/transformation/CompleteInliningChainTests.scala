package de.tu_darmstadt.veritas.backend.transformation

import org.scalatest.FunSuite
import de.tu_darmstadt.veritas.backend.veritas.TypingRule
import de.tu_darmstadt.veritas.backend.veritas.Axioms
import de.tu_darmstadt.veritas.backend.veritas.Module
import de.tu_darmstadt.veritas.backend.veritas.MetaVar
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionMeta
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpApp
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpJudgment
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpEq
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpMeta
import de.tu_darmstadt.veritas.backend.transformation.defs.InlineEverythingFPAndRemovePrems
import de.tu_darmstadt.veritas.backend.transformation.defs.CollectOnly
import de.tu_darmstadt.veritas.backend.transformation.defs.InlineEverythingOnce
import de.tu_darmstadt.veritas.backend.transformation.defs.InlineEverythingFP
import de.tu_darmstadt.veritas.backend.Backend
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpFalse

/**
 * tests the complete inlining transformation chain
 */
class CompleteInliningChainTests extends FunSuite {

  def genSimpleModule(name: String, testRule: TypingRule) =
    Module(name, Seq(), Seq(Axioms(Seq(testRule))))

  def genMeta(name: String) = FunctionMeta(MetaVar(name))

  def genEq(left: FunctionExpMeta, right: FunctionExpMeta) =
    FunctionExpJudgment(FunctionExpEq(left, right))

  def genApp1(fn: String, meta: String) =
    FunctionExpApp(fn, Seq(genMeta(meta)))

  def genAppApp1(fn: String, arg: FunctionExpMeta) =
    FunctionExpApp(fn, Seq(arg))

  test("Simple implication inlining premise into conclusion") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genApp1("f", "y"))),
      Seq(genEq(genApp1("g", "z"), genMeta("x"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(),
      Seq(genEq(genApp1("g", "z"), genApp1("f", "y"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingFPAndRemovePrems(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Simple implication inlining premise into conclusion other way around") {
    val tr = TypingRule("simple",
      Seq(genEq(genApp1("f", "y"), genMeta("x"))),
      Seq(genEq(genApp1("g", "z"), genMeta("x"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(),
      Seq(genEq(genApp1("g", "z"), genApp1("f", "y"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingFPAndRemovePrems(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("No inlining of circular equations") {
    val tr = TypingRule("simple",
      Seq(genEq(genApp1("f", "x"), genMeta("x"))),
      Seq(genEq(genApp1("g", "z"), genMeta("x"))))
    val mod = genSimpleModule("inlinesimple", tr)
s
    val res = InlineEverythingFPAndRemovePrems(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == mod)
  }

  test("Inlining within premises") {
    val tr = TypingRule("simple",
      Seq(genEq(genApp1("f", "y"), genMeta("x")), genEq(genMeta("z"), genApp1("f", "x"))),
      Seq(FunctionExpJudgment(FunctionExpFalse)))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(genEq(genMeta("z"), genAppApp1("f", genApp1("f", "y")))),
      Seq(FunctionExpJudgment(FunctionExpFalse)))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingFPAndRemovePrems(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Inlining within premises 'wrong' order") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("z"), genApp1("f", "x")), genEq(genApp1("f", "y"), genMeta("x"))),
      Seq(FunctionExpJudgment(FunctionExpFalse)))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(genEq(genMeta("z"), genAppApp1("f", genApp1("f", "y")))),
      Seq(FunctionExpJudgment(FunctionExpFalse)))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingFPAndRemovePrems(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Inlining within conclusion") {
    val tr = TypingRule("simple",
      Seq(),
      Seq(genEq(genApp1("f", "y"), genMeta("x")), genEq(genMeta("z"), genApp1("f", "x"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(),
      Seq(genEq(genMeta("z"), genAppApp1("f", genApp1("f", "y")))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingFPAndRemovePrems(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Inlining within conclusion 'wrong' order") {
    val tr = TypingRule("simple",
      Seq(),
      Seq(genEq(genMeta("z"), genApp1("f", "x")), genEq(genApp1("f", "y"), genMeta("x"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(),
      Seq(genEq(genMeta("z"), genAppApp1("f", genApp1("f", "y")))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingFPAndRemovePrems(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("No inlining in wrong direction (conclusion into premises)") {
    val tr = TypingRule("simple",
      Seq(genEq(genApp1("g", "z"), genApp1("f", "x"))),
      Seq(genEq(genMeta("x"), genApp1("f", "y"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val res = InlineEverythingFPAndRemovePrems(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == mod)
  }


  test("Inlining of metavar-metavar eq simple from left to right, one step (no removal)") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("y"), genMeta("x"))),
      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(genEq(genMeta("y"), genMeta("x"))),
      Seq(genEq(genApp1("g", "z"), genMeta("x"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingOnce(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Inlining and removal of metavar-metavar eq simple from left to right") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("y"), genMeta("x"))),
      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(),
      Seq(genEq(genApp1("g", "z"), genMeta("x"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingFPAndRemovePrems(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("No inlining of metavar-metavar eq simple from right to left, but remove") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("y"))),
      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(),
      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingFPAndRemovePrems(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Inlining and removal of metavar-metavar eq simple, several equations") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("y")),
        genEq(genMeta("y"), genMeta("z"))),
      Seq(genEq(genApp1("g", "a"), genMeta("x"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(),
      Seq(genEq(genApp1("g", "a"), genMeta("z"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingFPAndRemovePrems(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Cascading substitutions simple applications 1, first step") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("y")),
        genEq(genApp1("f", "x"), genMeta("z"))),
      Seq(genEq(genApp1("g", "a"), genMeta("z"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("y")),
        genEq(genApp1("f", "y"), genMeta("z"))),
      Seq(genEq(genApp1("g", "a"), genApp1("f", "x"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingOnce(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Cascading substitutions simple applications 1, second step") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("y")),
        genEq(genApp1("f", "y"), genMeta("z"))),
      Seq(genEq(genApp1("g", "a"), genApp1("f", "x"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("y")),
        genEq(genApp1("f", "y"), genMeta("z"))),
      Seq(genEq(genApp1("g", "a"), genApp1("f", "y"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingOnce(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }
  
  test("Cascading substitutions simple applications 1, all steps") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("y")),
        genEq(genApp1("f", "x"), genMeta("z"))),
      Seq(genEq(genApp1("g", "a"), genMeta("z"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("y")),
        genEq(genApp1("f", "y"), genMeta("z"))),
      Seq(genEq(genApp1("g", "a"), genApp1("f", "y"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingFP(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Cascading substitutions simple applications 1") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("y")),
        genEq(genApp1("f", "x"), genMeta("z"))),
      Seq(genEq(genApp1("g", "a"), genMeta("z"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(),
      Seq(genEq(genApp1("g", "a"), genApp1("f", "y"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingFPAndRemovePrems(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Cascading substitutions simple applications 2") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("y")),
        genEq(genApp1("f", "y"), genMeta("z"))),
      Seq(genEq(genApp1("g", "a"), genMeta("z"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("y"))),
      Seq(genEq(genApp1("g", "a"), genApp1("f", "y"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingFPAndRemovePrems(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Inlining metavar-metavar eq circular (do nothing but remove)") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("y"), genMeta("x")),
        genEq(genMeta("x"), genMeta("y"))),
      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(),
      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineEverythingFPAndRemovePrems(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

}