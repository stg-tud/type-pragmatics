package de.tu_darmstadt.veritas.backend.transformation

import org.scalatest.FunSuite
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.transformation.defs._
import de.tu_darmstadt.veritas.backend.Backend

/**
 * tests just the collection stage of the inlining transformation chain
 */
class CollectInlineEquationsTest extends FunSuite {
  def genSimpleModule(name: String, testRule: TypingRule) =
    Module(name, Seq(), Seq(Axioms(Seq(testRule))))

  def genMeta(name: String) = FunctionMeta(MetaVar(name))

  def genEq(left: FunctionExpMeta, right: FunctionExpMeta) =
    FunctionExpJudgment(FunctionExpEq(left, right))

  def genApp1(fn: String, meta: String) =
    FunctionExpApp(fn, Seq(genMeta(meta)))

  def genAppApp1(fn: String, arg: FunctionExpMeta) =
    FunctionExpApp(fn, Seq(arg))

  test("Collect only: Inlining within premises 'wrong' order") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("z"), genApp1("f", "x")), genEq(genApp1("f", "y"), genMeta("x"))),
      Seq(FunctionExpJudgment(FunctionExpFalse)))
    val mod = genSimpleModule("inlinesimple", tr)

    val collection = CollectOnly(Seq(mod))(Backend.onlyTFFTest)

    assert(collection.head == mod)
    assert(CollectOnly.chosenSubstitutions(MetaVar("z")) == genApp1("f", "x"))
    assert(CollectOnly.chosenSubstitutions(MetaVar("x")) == genApp1("f", "y"))
    assert(CollectOnly.chosenSubstitutions.size == 2)
  }

  test("Collect substitution equations metavar-metavar eq simple from left to right") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("y"), genMeta("x"))),
      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val collection = CollectOnly(Seq(mod))(Backend.onlyTFFTest)

    assert(collection.head == mod)
    assert(CollectOnly.chosenSubstitutions(MetaVar("y")) == genMeta("x"))
    assert(CollectOnly.chosenSubstitutions.size == 1)
  }

}