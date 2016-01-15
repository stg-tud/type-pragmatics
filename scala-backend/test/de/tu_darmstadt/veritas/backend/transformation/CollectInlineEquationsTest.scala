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

  def genExists1(vars: List[String], arg: TypingRuleJudgment) =
    ExistsJudgment(vars map { s => MetaVar(s) }, Seq(arg))

  def genForall1(vars: List[String], arg: TypingRuleJudgment) =
    ForallJudgment(vars map { s => MetaVar(s) }, Seq(arg))

  test("Collect first instance only, if several given") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("a")),
        genEq(genMeta("x"), genMeta("b")),
        genEq(genMeta("x"), genMeta("c"))),
      Seq(FunctionExpJudgment(FunctionExpFalse)))
    val mod = genSimpleModule("inlinesimple", tr)

    val collection = CollectOnly(Seq(mod))(Backend.onlyTFFTest)

    assert(collection.head == mod)
    assert(CollectOnly.chosenSubstitutions(MetaVar("x")) == genMeta("a"))
    assert(CollectOnly.chosenSubstitutions.size == 1)
  }
  
  test("Collect first instance only, if several given (in conclusion)") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("a")),
        genEq(genMeta("x"), genMeta("b"))),
      Seq(genEq(genMeta("x"), genMeta("c"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val collection = CollectOnly(Seq(mod))(Backend.onlyTFFTest)

    assert(collection.head == mod)
    assert(CollectOnly.chosenSubstitutions(MetaVar("x")) == genMeta("a"))
    assert(CollectOnly.chosenSubstitutions.size == 1)
  }
  
  test("Collect several substitutions, both directions") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("a")),
        genEq(genApp1("g", "z"), genMeta("b")),
        genEq(genMeta("z"), genMeta("c"))),
      Seq(FunctionExpJudgment(FunctionExpFalse)))
    val mod = genSimpleModule("inlinesimple", tr)

    val collection = CollectOnly(Seq(mod))(Backend.onlyTFFTest)

    assert(collection.head == mod)
    assert(CollectOnly.chosenSubstitutions(MetaVar("x")) == genMeta("a"))
    assert(CollectOnly.chosenSubstitutions(MetaVar("b")) == genApp1("g", "z"))
    assert(CollectOnly.chosenSubstitutions(MetaVar("z")) == genMeta("c"))
    assert(CollectOnly.chosenSubstitutions.size == 3)
  }
  
  test("Collect several substitutions, both directions, with conclusion") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("a")),
        genEq(genApp1("g", "z"), genMeta("b")),
        genEq(genMeta("z"), genMeta("c"))),
      Seq(genEq(genMeta("y"), genMeta("x")),
          genEq(genMeta("c"), genMeta("b"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val collection = CollectOnly(Seq(mod))(Backend.onlyTFFTest)

    assert(collection.head == mod)
    assert(CollectOnly.chosenSubstitutions(MetaVar("x")) == genMeta("a"))
    assert(CollectOnly.chosenSubstitutions(MetaVar("b")) == genApp1("g", "z"))
    assert(CollectOnly.chosenSubstitutions(MetaVar("z")) == genMeta("c"))
    assert(CollectOnly.chosenSubstitutions(MetaVar("y")) == genMeta("x"))
    assert(CollectOnly.chosenSubstitutions(MetaVar("c")) == genMeta("b"))
    assert(CollectOnly.chosenSubstitutions.size == 5)
  }

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

  test("Collect in conclusion") {
    val tr = TypingRule("simple",
      Seq(),
      Seq(genEq(genMeta("y"), genMeta("x")),
        genEq(genApp1("g", "z"), genMeta("y"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val collection = CollectOnly(Seq(mod))(Backend.onlyTFFTest)

    assert(collection.head == mod)
    assert(CollectOnly.chosenSubstitutions(MetaVar("y")) == genMeta("x"))
    assert(CollectOnly.chosenSubstitutions.size == 1)
  }

  test("Collect two things in conclusion") {
    val tr = TypingRule("simple",
      Seq(),
      Seq(genEq(genMeta("y"), genMeta("x")),
        genEq(genApp1("g", "z"), genMeta("a"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val collection = CollectOnly(Seq(mod))(Backend.onlyTFFTest)

    assert(collection.head == mod)
    assert(CollectOnly.chosenSubstitutions(MetaVar("y")) == genMeta("x"))
    assert(CollectOnly.chosenSubstitutions(MetaVar("a")) == genApp1("g", "z"))
    assert(CollectOnly.chosenSubstitutions.size == 2)
  }

  test("Correctly collect under presence of exists") {
    val tr = TypingRule("simple",
      Seq(genExists1(List("x", "z"), genEq(genMeta("x"), genMeta("z"))),
        genEq(genMeta("x"), genMeta("y"))),
      Seq(FunctionExpJudgment(FunctionExpFalse)))
    val mod = genSimpleModule("inlinesimple", tr)

    val collection = CollectOnly(Seq(mod))(Backend.onlyTFFTest)

    assert(collection.head == mod)
    assert(CollectOnly.chosenSubstitutions(MetaVar("x")) == genMeta("y"))
    assert(CollectOnly.chosenSubstitutions.size == 1)
  }

  test("Correctly collect under presence of forall") {
    val tr = TypingRule("simple",
      Seq(genForall1(List("x", "z"), genEq(genMeta("x"), genMeta("z"))),
        genEq(genMeta("x"), genMeta("y"))),
      Seq(FunctionExpJudgment(FunctionExpFalse)))
    val mod = genSimpleModule("inlinesimple", tr)

    val collection = CollectOnly(Seq(mod))(Backend.onlyTFFTest)

    assert(collection.head == mod)
    assert(CollectOnly.chosenSubstitutions(MetaVar("x")) == genMeta("y"))
    assert(CollectOnly.chosenSubstitutions.size == 1)
  }
  
  test("Correctly collect under presence of not") {
    val tr = TypingRule("simple",
      Seq(NotJudgment(genEq(genMeta("x"), genMeta("z"))),
        genEq(genMeta("x"), genMeta("y"))),
      Seq(FunctionExpJudgment(FunctionExpFalse)))
    val mod = genSimpleModule("inlinesimple", tr)

    val collection = CollectOnly(Seq(mod))(Backend.onlyTFFTest)

    assert(collection.head == mod)
    assert(CollectOnly.chosenSubstitutions(MetaVar("x")) == genMeta("y"))
    assert(CollectOnly.chosenSubstitutions.size == 1)
  }
}