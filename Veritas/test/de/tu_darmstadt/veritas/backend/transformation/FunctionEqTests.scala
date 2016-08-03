package de.tu_darmstadt.veritas.backend.transformation

import org.scalatest.FunSuite
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.ast.Module
import de.tu_darmstadt.veritas.backend.ast.Functions
import de.tu_darmstadt.veritas.backend.ast.function.FunctionSig
import de.tu_darmstadt.veritas.backend.ast.SortRef
import de.tu_darmstadt.veritas.backend.ast.function.FunctionEq
import de.tu_darmstadt.veritas.backend.ast.function.FunctionPatVar
import de.tu_darmstadt.veritas.backend.ast.function.FunctionPatApp
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpApp
import de.tu_darmstadt.veritas.backend.ast.function.FunctionPattern
import de.tu_darmstadt.veritas.backend.ast.ModuleDef
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpEq
import de.tu_darmstadt.veritas.backend.ast.TypingRule
import de.tu_darmstadt.veritas.backend.ast.Axioms
import de.tu_darmstadt.veritas.backend.ast.FunctionExpJudgment
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpMeta
import de.tu_darmstadt.veritas.backend.transformation.defs.FunctionEqToAxiomsSimple
import de.tu_darmstadt.veritas.backend.Backend
import de.tu_darmstadt.veritas.backend.transformation.lowlevel.VarToApp0
import de.tu_darmstadt.veritas.backend.ast.DataType
import de.tu_darmstadt.veritas.backend.ast.DataTypeConstructor
import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function.FunctionMeta
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpNeq
import de.tu_darmstadt.veritas.backend.ast.OrJudgment

class FunctionEqTests extends FunSuite {

  val natDatatype = DataType(false, "nat", Seq(
    DataTypeConstructor("zero", Seq()),
    DataTypeConstructor("succ", Seq(SortRef("nat")))))

  def genSimpleFunModule(name: String, testFunction: FunctionDef) =
    Module(name, Seq(), Seq(natDatatype, Functions(Seq(testFunction))))

  def genSimpleModule(name: String, moddefs: Seq[ModuleDef]) =
    Module(name, Seq(), natDatatype +: moddefs)

  def nat1FunctionSig(name: String) = FunctionSig(name, Seq(SortRef("nat")), SortRef("nat"))
  def nat2FunctionSig(name: String) = FunctionSig(name, Seq(SortRef("nat"), SortRef("nat")), SortRef("nat"))

  def genMeta(name: String) = FunctionMeta(MetaVar(name))

  val zeroPat = FunctionPatVar("zero")
  val zeroExp = FunctionExpApp("zero", Seq())
  def succPat(p: FunctionPattern) = FunctionPatApp("succ", Seq(p))
  def succExp(p: FunctionExpMeta) = FunctionExpApp("succ", Seq(p))

  def genApp1(fn: String, arg: FunctionExpMeta) =
    FunctionExpApp(fn, Seq(arg))

  def genApp2(fn: String, arg1: FunctionExpMeta, arg2: FunctionExpMeta) =
    FunctionExpApp(fn, Seq(arg1, arg2))

  def genEq(left: FunctionExpMeta, right: FunctionExpMeta) =
    FunctionExpJudgment(FunctionExpEq(left, right))

  def genNeq(left: FunctionExpMeta, right: FunctionExpMeta) =
    FunctionExpJudgment(FunctionExpNeq(left, right))

  def genSimpleEqRule(name: String, f: FunctionExpJudgment) =
    TypingRule(name, Seq(), Seq(f))

  test("Simple function with non-overlapping patterns") {
    val func = FunctionDef(nat1FunctionSig("f"),
      Seq(FunctionEq("f", Seq(zeroPat), zeroExp),
        FunctionEq("f", Seq(succPat(zeroPat)), zeroExp)))

    val mod = genSimpleFunModule("test", func)

    val resaxs = Axioms(Seq(
      genSimpleEqRule("f0", genEq(genApp1("f", zeroExp), zeroExp)),
      genSimpleEqRule("f1", genEq(genApp1("f", succExp(zeroExp)), zeroExp))))

    val modres = genSimpleModule("test",
      Seq(Functions(Seq(FunctionDef(nat1FunctionSig("f"), Seq()))),
        resaxs))

    val res = FunctionEqToAxiomsSimple(VarToApp0(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)

  }

  test("Simple function with one overlapping pattern") {
    val func = FunctionDef(nat1FunctionSig("f"),
      Seq(FunctionEq("f", Seq(zeroPat), zeroExp),
        FunctionEq("f", Seq(FunctionPatVar("n")), zeroExp)))

    val mod = genSimpleFunModule("test", func)

    val resaxs = Axioms(Seq(
      genSimpleEqRule("f0", genEq(genApp1("f", zeroExp), zeroExp)),
      TypingRule("f1", Seq(genNeq(genMeta("n"), zeroExp)),
        Seq(genEq(genApp1("f", genMeta("n")), zeroExp)))))

    val modres = genSimpleModule("test",
      Seq(Functions(Seq(FunctionDef(nat1FunctionSig("f"), Seq()))),
        resaxs))

    val res = FunctionEqToAxiomsSimple(VarToApp0(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)

  }

  test("Simple function with two overlapping patterns") {
    val func = FunctionDef(nat1FunctionSig("f"),
      Seq(FunctionEq("f", Seq(zeroPat), zeroExp),
        FunctionEq("f", Seq(succPat(zeroPat)), zeroExp),
        FunctionEq("f", Seq(FunctionPatVar("n")), zeroExp)))

    val mod = genSimpleFunModule("test", func)

    val resaxs = Axioms(Seq(
      genSimpleEqRule("f0", genEq(genApp1("f", zeroExp), zeroExp)),
      genSimpleEqRule("f1", genEq(genApp1("f", succExp(zeroExp)), zeroExp)),
      TypingRule("f2", Seq(genNeq(genMeta("n"), zeroExp), genNeq(genMeta("n"), succExp(zeroExp))),
        Seq(genEq(genApp1("f", genMeta("n")), zeroExp)))))

    val modres = genSimpleModule("test",
      Seq(Functions(Seq(FunctionDef(nat1FunctionSig("f"), Seq()))),
        resaxs))

    val res = FunctionEqToAxiomsSimple(VarToApp0(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)

  }

  test("Two argument function, no overlapping patterns") {
    val func = FunctionDef(nat2FunctionSig("f"),
      Seq(FunctionEq("f", Seq(zeroPat, zeroPat), zeroExp),
        FunctionEq("f", Seq(zeroPat, succPat(zeroPat)), zeroExp),
        FunctionEq("f", Seq(succPat(zeroPat), succPat(zeroPat)), zeroExp)))

    val mod = genSimpleFunModule("test", func)

    val resaxs = Axioms(Seq(
      genSimpleEqRule("f0", genEq(genApp2("f", zeroExp, zeroExp), zeroExp)),
      genSimpleEqRule("f1", genEq(genApp2("f", zeroExp, succExp(zeroExp)), zeroExp)),
      genSimpleEqRule("f2", genEq(genApp2("f", succExp(zeroExp), succExp(zeroExp)), zeroExp))))

    val modres = genSimpleModule("test",
      Seq(Functions(Seq(FunctionDef(nat2FunctionSig("f"), Seq()))),
        resaxs))

    val res = FunctionEqToAxiomsSimple(VarToApp0(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)

  }

  test("Two argument function, no overlapping patterns (example from comment)") {
    val func = FunctionDef(nat2FunctionSig("f"),
      Seq(FunctionEq("f", Seq(zeroPat, succPat(zeroPat)), FunctionExpApp("x", Seq())),
        FunctionEq("f", Seq(succPat(FunctionPatVar("n")), FunctionPatVar("n")), FunctionExpApp("y", Seq()))))

    val mod = genSimpleFunModule("test", func)

    val resaxs = Axioms(Seq(
      genSimpleEqRule("f0", genEq(genApp2("f", zeroExp, succExp(zeroExp)), FunctionExpApp("x", Seq()))),
      genSimpleEqRule("f1", genEq(genApp2("f", succExp(genMeta("n")), genMeta("n")), FunctionExpApp("y", Seq())))))

    val modres = genSimpleModule("test",
      Seq(Functions(Seq(FunctionDef(nat2FunctionSig("f"), Seq()))),
        resaxs))

    val res = FunctionEqToAxiomsSimple(VarToApp0(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)

  }

  test("Two argument function, no overlapping patterns, but more tricky") {
    val func = FunctionDef(nat2FunctionSig("f"),
      Seq(FunctionEq("f", Seq(zeroPat, zeroPat), zeroExp),
        FunctionEq("f", Seq(zeroPat, succPat(zeroPat)), zeroExp),
        FunctionEq("f", Seq(succPat(zeroPat), FunctionPatVar("n")), zeroExp)))

    val mod = genSimpleFunModule("test", func)

    val resaxs = Axioms(Seq(
      genSimpleEqRule("f0", genEq(genApp2("f", zeroExp, zeroExp), zeroExp)),
      genSimpleEqRule("f1", genEq(genApp2("f", zeroExp, succExp(zeroExp)), zeroExp)),
      TypingRule("f2", Seq(),
        Seq(genEq(genApp2("f", succExp(zeroExp), genMeta("n")), zeroExp)))))

    val modres = genSimpleModule("test",
      Seq(Functions(Seq(FunctionDef(nat2FunctionSig("f"), Seq()))),
        resaxs))

    val res = FunctionEqToAxiomsSimple(VarToApp0(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)

  }

  test("Two argument function, one overlapping pattern") {
    val func = FunctionDef(nat2FunctionSig("f"),
      Seq(FunctionEq("f", Seq(zeroPat, zeroPat), zeroExp),
        FunctionEq("f", Seq(FunctionPatVar("n"), zeroPat), zeroExp)))

    val mod = genSimpleFunModule("test", func)

    val resaxs = Axioms(Seq(
      genSimpleEqRule("f0", genEq(genApp2("f", zeroExp, zeroExp), zeroExp)),
      TypingRule("f1", Seq(genNeq(genMeta("n"), zeroExp)),
        Seq(genEq(genApp2("f", genMeta("n"), zeroExp), zeroExp)))))

    val modres = genSimpleModule("test",
      Seq(Functions(Seq(FunctionDef(nat2FunctionSig("f"), Seq()))),
        resaxs))

    val res = FunctionEqToAxiomsSimple(VarToApp0(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)

  }

  test("Two argument function, two overlapping patterns") {
    val func = FunctionDef(nat2FunctionSig("f"),
      Seq(FunctionEq("f", Seq(zeroPat, zeroPat), zeroExp),
        FunctionEq("f", Seq(zeroPat, succPat(zeroPat)), zeroExp),
        FunctionEq("f", Seq(zeroPat, FunctionPatVar("n")), zeroExp)))

    val mod = genSimpleFunModule("test", func)

    val resaxs = Axioms(Seq(
      genSimpleEqRule("f0", genEq(genApp2("f", zeroExp, zeroExp), zeroExp)),
      genSimpleEqRule("f1", genEq(genApp2("f", zeroExp, succExp(zeroExp)), zeroExp)),
      TypingRule("f2", Seq(genNeq(genMeta("n"), zeroExp), genNeq(genMeta("n"), succExp(zeroExp))),
        Seq(genEq(genApp2("f", zeroExp, genMeta("n")), zeroExp)))))

    val modres = genSimpleModule("test",
      Seq(Functions(Seq(FunctionDef(nat2FunctionSig("f"), Seq()))),
        resaxs))

    val res = FunctionEqToAxiomsSimple(VarToApp0(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)

  }

  test("Two argument function, two overlapping patterns in one equation") {
    val func = FunctionDef(nat2FunctionSig("f"),
      Seq(FunctionEq("f", Seq(zeroPat, zeroPat), zeroExp),
        FunctionEq("f", Seq(FunctionPatVar("n"), FunctionPatVar("m")), zeroExp)))

    val mod = genSimpleFunModule("test", func)

    val resaxs = Axioms(Seq(
      genSimpleEqRule("f0", genEq(genApp2("f", zeroExp, zeroExp), zeroExp)),
      TypingRule("f1", Seq(OrJudgment(Seq(
        Seq(genNeq(genMeta("n"), zeroExp)),
        Seq(genNeq(genMeta("m"), zeroExp))))),
        Seq(genEq(genApp2("f", genMeta("n"), genMeta("m")), zeroExp)))))

    val modres = genSimpleModule("test",
      Seq(Functions(Seq(FunctionDef(nat2FunctionSig("f"), Seq()))),
        resaxs))

    val res = FunctionEqToAxiomsSimple(VarToApp0(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)

  }

  test("Two argument function, mixing overlapping patterns and not overlapping ones") {
    val func = FunctionDef(nat2FunctionSig("f"),
      Seq(FunctionEq("f", Seq(zeroPat, zeroPat), zeroExp),
        FunctionEq("f", Seq(zeroPat, FunctionPatVar("n")), zeroExp),
        FunctionEq("f", Seq(succPat(zeroPat), FunctionPatVar("n")), zeroExp),
        FunctionEq("f", Seq(FunctionPatVar("n"), FunctionPatVar("m")), zeroExp)))

    val mod = genSimpleFunModule("test", func)

    //note: current result of this transformation is unoptimized!
    val resaxs = Axioms(Seq(
      genSimpleEqRule("f0", genEq(genApp2("f", zeroExp, zeroExp), zeroExp)),
      TypingRule("f1", Seq(genNeq(genMeta("n"), zeroExp)),
        Seq(genEq(genApp2("f", zeroExp, genMeta("n")), zeroExp))),
      genSimpleEqRule("f2", genEq(genApp2("f", succExp(zeroExp), genMeta("n")), zeroExp)),
      TypingRule("f3", Seq(OrJudgment(Seq(
          Seq(genNeq(genMeta("n"), zeroExp)), 
          Seq(genNeq(genMeta("m"), zeroExp)))),
          genNeq(genMeta("n"), zeroExp),
        genNeq(genMeta("n"), succExp(zeroExp))),
        Seq(genEq(genApp2("f", genMeta("n"), genMeta("m")), zeroExp)))))

    val modres = genSimpleModule("test",
      Seq(Functions(Seq(FunctionDef(nat2FunctionSig("f"), Seq()))),
        resaxs))

    val res = FunctionEqToAxiomsSimple(VarToApp0(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)

    assert(res.head == modres)

  }

}