package de.tu_darmstadt.veritas.backend.transformation

import org.scalatest.FunSuite
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.Backend
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpTrue

class SplitGoalTest extends FunSuite {

  def genTrueRule(name: String) = TypingRule("test1", Seq(), Seq(FunctionExpJudgment(FunctionExpTrue)))

  test("No goal duplication - single goal") {
    val mod = Module("test", Seq(), Seq(Goals(Seq(genTrueRule("test")), None)))

    val res = SplitModulesByGoal()(Seq(mod))(Backend.onlyTFFTest)

    assert(res.length == 1)
  }

  test("No goal duplication - module with 2 goals") {
    val mod = Module("test", Seq(), Seq(Goals(Seq(genTrueRule("test1"), genTrueRule("test2")), None)))
    val res = SplitModulesByGoal()(Seq(mod))(Backend.onlyTFFTest)

    assert(res.length == 2)
  }

  test("No goal duplication - single local block") {
    val mod = Module("test", Seq(), Seq(
      Local(Seq(
        Goals(Seq(genTrueRule("test")), None)))))

    val res = SplitModulesByGoal()(Seq(mod))(Backend.onlyTFFTest)

    assert(res.length == 1)
  }

  test("No goal duplication - single goal plus single local block") {
    val mod = Module("test", Seq(), Seq(
      Goals(Seq(genTrueRule("test-single")), None),
      Local(Seq(
        Goals(Seq(genTrueRule("test")), None)))))

    val res = SplitModulesByGoal()(Seq(mod))(Backend.onlyTFFTest)

    assert(res.length == 2)
  }

  test("No goal duplication - several local blocks") {
    val mod = Module("test", Seq(), Seq(
      Local(Seq(
        Goals(Seq(genTrueRule("test")), None))),
      Local(Seq(
        Goals(Seq(genTrueRule("test")), None))),
      Local(Seq(
        Goals(Seq(genTrueRule("test")), None)))))

    val res = SplitModulesByGoal()(Seq(mod))(Backend.onlyTFFTest)

    assert(res.length == 3)
  }

  test("No goal duplication - several local blocks with axioms and different consts") {
    val mod = Module("test", Seq(), Seq(
      Local(Seq(
        Consts(Seq(ConstDecl("x", SortRef("t"))), true),
        Axioms(Seq(genTrueRule("true"))),
        Goals(Seq(genTrueRule("test")), None))),
      Local(Seq(
        Consts(Seq(ConstDecl("x", SortRef("t")), ConstDecl("y", SortRef("t"))), true),
        Goals(Seq(genTrueRule("test")), None))),
      Local(Seq(
        Axioms(Seq(genTrueRule("true"))),
        Goals(Seq(genTrueRule("test")), None)))))

    val res = SplitModulesByGoal()(Seq(mod))(Backend.onlyTFFTest)

    assert(res.length == 3)
  }

}
