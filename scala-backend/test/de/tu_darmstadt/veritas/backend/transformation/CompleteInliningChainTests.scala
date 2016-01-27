package de.tu_darmstadt.veritas.backend.transformation

import org.scalatest.FunSuite
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.transformation.defs.InlineOnce
import de.tu_darmstadt.veritas.backend.transformation.defs.InlineFP
import de.tu_darmstadt.veritas.backend.transformation.lowlevel.LogicalTermOptimization
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

    val res = LogicalTermOptimization(InlineFP(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

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

    val res = LogicalTermOptimization(InlineFP(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("No inlining of circular equations") {
    val tr = TypingRule("simple",
      Seq(genEq(genApp1("f", "x"), genMeta("x"))),
      Seq(genEq(genApp1("g", "z"), genMeta("x"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val res = LogicalTermOptimization(InlineFP(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == mod)
  }

  test("Inlining within premises") {
    val tr = TypingRule("simple",
      Seq(genEq(genApp1("f", "y"), genMeta("x")), genEq(genMeta("z"), genApp1("f", "x"))),
      Seq(genEq(genMeta("a"), genMeta("z"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(),
      Seq(genEq(genMeta("a"), genAppApp1("f", genApp1("f", "y")))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = LogicalTermOptimization(InlineFP(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Inlining within premises, different order") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("z"), genApp1("f", "x")),
        genEq(genApp1("f", "y"), genMeta("x"))),
      Seq(genEq(genMeta("a"), genMeta("z"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(),
      Seq(genEq(genMeta("a"), genAppApp1("f", genApp1("f", "y")))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = LogicalTermOptimization(InlineFP(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("No inlining in wrong direction (conclusion into premises)") {
    val tr = TypingRule("simple",
      Seq(genEq(genApp1("g", "z"), genApp1("f", "x"))),
      Seq(genEq(genMeta("x"), genApp1("f", "y"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val res = LogicalTermOptimization(InlineFP(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == mod)
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

    val res = LogicalTermOptimization(InlineFP(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

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

    val res = LogicalTermOptimization(InlineFP(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

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

    val res = LogicalTermOptimization(InlineFP(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)
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

    val res = LogicalTermOptimization(InlineFP(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Cascading substitutions simple applications 2") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("y")),
        genEq(genApp1("f", "y"), genMeta("z"))),
      Seq(genEq(genApp1("g", "a"), genMeta("z"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(),
      Seq(genEq(genApp1("g", "a"), genApp1("f", "y"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = LogicalTermOptimization(InlineFP(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Inlining metavar-metavar eq circular") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("y"), genMeta("x")),
        genEq(genMeta("x"), genMeta("y"))),
      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(),
      Seq(genEq(genApp1("g", "z"), genMeta("x"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = LogicalTermOptimization(InlineFP(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Capture avoidance for quantifiers") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genApp1("f", "a")),
        ForallJudgment(Seq(MetaVar("x")),
          Seq(genEq(genMeta("x"), genApp1("f", "y")),
            ExistsJudgment(Seq(MetaVar("x")),
              Seq(genEq(genMeta("x"), genApp1("g", "s"))))))),
      Seq(genEq(genApp1("g", "z"), genMeta("x"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(ForallJudgment(Seq(MetaVar("x0")),
          Seq(genEq(genMeta("x0"), genApp1("f", "y")),
            ExistsJudgment(Seq(MetaVar("x10")),
              Seq(genEq(genMeta("x10"), genApp1("g", "s"))))))),
      Seq(genEq(genApp1("g", "z"), genApp1("f", "a"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = LogicalTermOptimization(InlineFP(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)

  }

}