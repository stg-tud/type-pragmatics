package de.tu_darmstadt.veritas.backend.transformation

import org.scalatest.FunSuite

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.transformation.lowlevel.VarToApp0
import de.tu_darmstadt.veritas.backend.transformation.defs.FunctionEqToAxiomsSimple
import de.tu_darmstadt.veritas.backend.transformation.defs.TotalFunctionInversionAxioms
import de.tu_darmstadt.veritas.backend.Backend
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpFalse

class InversionAxiomsTest extends FunSuite {

  val natDatatype = DataType(false, "nat", Seq(
    DataTypeConstructor("zero", Seq()),
    DataTypeConstructor("succ", Seq(SortRef("nat")))))

  def genSimpleModule(name: String, moddefs: Seq[ModuleDef]) =
    Module(name, Seq(), natDatatype +: moddefs)

  def genSimpleFunModule(name: String, testFunction: FunctionDef) =
    Module(name, Seq(), Seq(natDatatype, Functions(Seq(testFunction))))

  def genSimpleParFunModule(name: String, testFunction: FunctionDef) =
    Module(name, Seq(), Seq(natDatatype, PartialFunctions(Seq(testFunction))))

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

  test("No inversion axiom for partial functions with several equations") {

    val func = FunctionDef(nat1FunctionSig("f"),
      Seq(FunctionEq("f", Seq(zeroPat), zeroExp),
        FunctionEq("f", Seq(FunctionPatVar("n")), zeroExp)))

    val mod = genSimpleParFunModule("test", func)

    val res = FunctionEqToAxiomsSimple(VarToApp0(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)
    val resinv = TotalFunctionInversionAxioms(res)(Backend.onlyTFFTest)

    val resaxs = Axioms(Seq(
      genSimpleEqRule("f0", genEq(genApp1("f", zeroExp), zeroExp)),
      TypingRule("f1", Seq(genNeq(genMeta("n"), zeroExp)),
        Seq(genEq(genApp1("f", genMeta("n")), zeroExp)))))

    val modres = genSimpleModule("test",
      Seq(PartialFunctions(Seq(FunctionDef(nat1FunctionSig("f"), Seq()))),
        resaxs))

    assert(resinv.head == modres)
  }

  test("Inversion axiom for functions with one equation") {

    val func = FunctionDef(nat1FunctionSig("f"),
      Seq(FunctionEq("f", Seq(zeroPat), zeroExp)))

    val mod = genSimpleFunModule("test", func)

    val res = FunctionEqToAxiomsSimple(VarToApp0(Seq(mod))(Backend.onlyTFFTest))(Backend.onlyTFFTest)
    val resinv = TotalFunctionInversionAxioms(res)(Backend.onlyTFFTest)

    val resaxs = Axioms(Seq(
      genSimpleEqRule("f0", genEq(genApp1("f", zeroExp), zeroExp))))

    val resinvaxs = Axioms(Seq(TypingRule("f-INV",
      Seq(genEq(genApp1("f", genMeta("nat0")), genMeta("RESULT"))),
      Seq(genEq(genMeta("nat0"), zeroExp), genEq(genMeta("RESULT"), zeroExp)))))

    val modres = genSimpleModule("test",
      Seq(Functions(Seq(FunctionDef(nat1FunctionSig("f"), Seq()))),
        resaxs, resinvaxs))

    assert(resinv.head == modres)
  }

}