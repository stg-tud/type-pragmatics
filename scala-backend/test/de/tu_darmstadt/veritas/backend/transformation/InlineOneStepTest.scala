package de.tu_darmstadt.veritas.backend.transformation

import org.scalatest.FunSuite
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.transformation.defs._
import de.tu_darmstadt.veritas.backend.Backend

/**
 * tests collection + substitution, but without removal
 * also test which variables are declared as "removable
 */
class InlineOneStepTest extends FunSuite {

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
    
  // tests without removal of premises  

  test("Simple implication inlining premise into conclusion") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genApp1("f", "y"))),
      Seq(genEq(genApp1("g", "z"), genMeta("x"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(FunctionExpJudgment(FunctionExpTrue)),
      Seq(genEq(genApp1("g", "z"), genApp1("f", "y"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineOnce(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Simple implication inlining premise into conclusion other way around") {
    val tr = TypingRule("simple",
      Seq(genEq(genApp1("f", "y"), genMeta("x"))),
      Seq(genEq(genApp1("g", "z"), genMeta("x"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(FunctionExpJudgment(FunctionExpTrue)),
      Seq(genEq(genApp1("g", "z"), genApp1("f", "y"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineOnce(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("No inlining of circular equations") {
    val tr = TypingRule("simple",
      Seq(genEq(genApp1("f", "x"), genMeta("x"))),
      Seq(genEq(genApp1("g", "z"), genMeta("x"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val res = InlineOnce(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == mod)
  }

  test("Inlining within premises") {
    val tr = TypingRule("simple",
      Seq(genEq(genApp1("f", "y"), genMeta("x")),
        genEq(genMeta("z"), genApp1("f", "x"))),
      Seq(FunctionExpJudgment(FunctionExpFalse)))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(FunctionExpJudgment(FunctionExpTrue), 
          genEq(genMeta("z"), genAppApp1("f", genApp1("f", "y")))),
      Seq(FunctionExpJudgment(FunctionExpFalse)))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineOnce(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("Inlining within conclusion") {
    val tr = TypingRule("simple",
      Seq(),
      Seq(genEq(genApp1("f", "y"), genMeta("x")),
        genEq(genMeta("z"), genApp1("f", "x"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(),
      Seq(FunctionExpJudgment(FunctionExpTrue),
          genEq(genMeta("z"), genAppApp1("f", genApp1("f", "y")))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineOnce(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("No inlining in wrong direction (conclusion into premises)") {
    val tr = TypingRule("simple",
      Seq(genEq(genApp1("g", "z"), genApp1("f", "x"))),
      Seq(genEq(genMeta("x"), genApp1("f", "y"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val res = InlineOnce(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == mod)
  }

  test("Inlining of metavar-metavar eq simple from left to right") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("y"), genMeta("x"))),
      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(FunctionExpJudgment(FunctionExpTrue)),
      Seq(genEq(genApp1("g", "z"), genMeta("x"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineOnce(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }

  test("No inlining of metavar-metavar eq simple from right to left") {
    val tr = TypingRule("simple",
      Seq(genEq(genMeta("x"), genMeta("y"))),
      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val trres = TypingRule("simple",
      Seq(FunctionExpJudgment(FunctionExpTrue)),
      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
    val modres = genSimpleModule("inlinesimple", trres)

    val res = InlineOnce(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == modres)
  }
  
  test("Do not throw away last equation in conclusion") {
    val tr = TypingRule("simple",
      Seq(),
      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
    val mod = genSimpleModule("inlinesimple", tr)

    val res = InlineOnce(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == mod)
  }
  
  test("Inlining in exists: do not throw away last equation") {
    val tr = TypingRule("simple",
      Seq(ExistsJudgment(Seq(MetaVar("x")), 
          Seq(genEq(genMeta("x"), genApp1("f", "y"))))),
      Seq(genEq(genApp1("g", "z"), genMeta("x"))))
    val mod = genSimpleModule("inlinesimple", tr)
    
    val res = InlineOnce(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == mod)
  }
  
  test("Inlining in forall: do not throw away last equation") {
    val tr = TypingRule("simple",
      Seq(ExistsJudgment(Seq(MetaVar("x")), 
          Seq(genEq(genMeta("x"), genApp1("f", "y"))))),
      Seq(genEq(genApp1("g", "z"), genMeta("x"))))
    val mod = genSimpleModule("inlinesimple", tr)
    
    val res = InlineOnce(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == mod)
  }
  
  test("Inlining in exists: binder renaming") {
    
  }
  

//  test("Inlining and removal of metavar-metavar eq simple, several equations") {
//    val tr = TypingRule("simple",
//      Seq(genEq(genMeta("x"), genMeta("y")),
//        genEq(genMeta("y"), genMeta("z"))),
//      Seq(genEq(genApp1("g", "a"), genMeta("x"))))
//    val mod = genSimpleModule("inlinesimple", tr)
//
//    val trres = TypingRule("simple",
//      Seq(genEq(genMeta("x"), genMeta("y")),
//        genEq(genMeta("y"), genMeta("z"))),
//      Seq(genEq(genApp1("g", "a"), genMeta("z"))))
//    val modres = genSimpleModule("inlinesimple", trres)
//
//    val res = InlineOnce(Seq(mod))(Backend.onlyTFFTest)
//
//    assert(res.head == modres)
//  }
//
//  test("Cascading substitutions simple applications 1, first step") {
//    val tr = TypingRule("simple",
//      Seq(genEq(genMeta("x"), genMeta("y")),
//        genEq(genApp1("f", "x"), genMeta("z"))),
//      Seq(genEq(genApp1("g", "a"), genMeta("z"))))
//    val mod = genSimpleModule("inlinesimple", tr)
//
//    val trres = TypingRule("simple",
//      Seq(genEq(genMeta("x"), genMeta("y")),
//        genEq(genApp1("f", "y"), genMeta("z"))),
//      Seq(genEq(genApp1("g", "a"), genApp1("f", "x"))))
//    val modres = genSimpleModule("inlinesimple", trres)
//
//    val res = InlineOnce(Seq(mod))(Backend.onlyTFFTest)
//
//    assert(res.head == modres)
//  }
//
//  test("Cascading substitutions simple applications 1, second step") {
//    val tr = TypingRule("simple",
//      Seq(genEq(genMeta("x"), genMeta("y")),
//        genEq(genApp1("f", "y"), genMeta("z"))),
//      Seq(genEq(genApp1("g", "a"), genApp1("f", "x"))))
//    val mod = genSimpleModule("inlinesimple", tr)
//
//    val trres = TypingRule("simple",
//      Seq(genEq(genMeta("x"), genMeta("y")),
//        genEq(genApp1("f", "y"), genMeta("z"))),
//      Seq(genEq(genApp1("g", "a"), genApp1("f", "y"))))
//    val modres = genSimpleModule("inlinesimple", trres)
//
//    val res = InlineOnce(Seq(mod))(Backend.onlyTFFTest)
//
//    assert(res.head == modres)
//  }

//  test("Cascading substitutions simple applications 1, all steps") {
//    val tr = TypingRule("simple",
//      Seq(genEq(genMeta("x"), genMeta("y")),
//        genEq(genApp1("f", "x"), genMeta("z"))),
//      Seq(genEq(genApp1("g", "a"), genMeta("z"))))
//    val mod = genSimpleModule("inlinesimple", tr)
//
//    val trres = TypingRule("simple",
//      Seq(genEq(genMeta("x"), genMeta("y")),
//        genEq(genApp1("f", "y"), genMeta("z"))),
//      Seq(genEq(genApp1("g", "a"), genApp1("f", "y"))))
//    val modres = genSimpleModule("inlinesimple", trres)
//
//    val res = InlineEverythingFP(Seq(mod))(Backend.onlyTFFTest)
//
//    assert(res.head == modres)
//    assert(InlineEverythingOnce.removableVars contains MetaVar("x"))
//    assert(InlineEverythingOnce.removableVars.size == 1)
//  }
//
//  test("Cascading substitutions simple applications 2, first step") {
//    val tr = TypingRule("simple",
//      Seq(genEq(genMeta("x"), genMeta("y")),
//        genEq(genApp1("f", "y"), genMeta("z"))),
//      Seq(genEq(genApp1("g", "a"), genMeta("z"))))
//    val mod = genSimpleModule("inlinesimple", tr)
//
//    val trres = TypingRule("simple",
//      Seq(genEq(genMeta("x"), genMeta("y")),
//        genEq(genApp1("f", "y"), genMeta("z"))),
//      Seq(genEq(genApp1("g", "a"), genApp1("f", "y"))))
//    val modres = genSimpleModule("inlinesimple", trres)
//
//    val res = InlineEverythingOnce(Seq(mod))(Backend.onlyTFFTest)
//
//    assert(res.head == modres)
//    assert(InlineEverythingOnce.removableVars contains MetaVar("z"))
//    assert(InlineEverythingOnce.removableVars.size == 1)
//  }
//
//  test("Cascading substitutions simple applications 2, second step (no change!)") {
//    val tr = TypingRule("simple",
//      Seq(genEq(genMeta("x"), genMeta("y")),
//        genEq(genApp1("f", "y"), genMeta("z"))),
//      Seq(genEq(genApp1("g", "a"), genApp1("f", "y"))))
//    val mod = genSimpleModule("inlinesimple", tr)
//
//    val res = InlineEverythingOnce(Seq(mod))(Backend.onlyTFFTest)
//
//    assert(res.head == mod)
//    assert(InlineEverythingOnce.removableVars contains MetaVar("x"))
//    assert(InlineEverythingOnce.removableVars.size == 1)
//  }
//
//  test("Cascading substitutions simple applications 2, all steps") {
//    val tr = TypingRule("simple",
//      Seq(genEq(genMeta("x"), genMeta("y")),
//        genEq(genApp1("f", "y"), genMeta("z"))),
//      Seq(genEq(genApp1("g", "a"), genMeta("z"))))
//    val mod = genSimpleModule("inlinesimple", tr)
//
//    val trres = TypingRule("simple",
//      Seq(genEq(genMeta("x"), genMeta("y")),
//        genEq(genApp1("f", "y"), genMeta("z"))),
//      Seq(genEq(genApp1("g", "a"), genApp1("f", "y"))))
//    val modres = genSimpleModule("inlinesimple", trres)
//
//    val res = InlineEverythingFP(Seq(mod))(Backend.onlyTFFTest)
//
//    assert(res.head == modres)
//    assert(InlineEverythingOnce.removableVars contains MetaVar("x"))
//    assert(InlineEverythingOnce.removableVars.size == 1)
//  }
//
//  test("Inlining metavar-metavar eq circular (do nothing)") {
//    val tr = TypingRule("simple",
//      Seq(genEq(genMeta("y"), genMeta("x")),
//        genEq(genMeta("x"), genMeta("y"))),
//      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
//    val mod = genSimpleModule("inlinesimple", tr)
//
//    val trres = TypingRule("simple",
//      Seq(genEq(genMeta("y"), genMeta("x")),
//        genEq(genMeta("x"), genMeta("y"))),
//      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
//    val modres = genSimpleModule("inlinesimple", trres)
//
//    val res = InlineEverythingOnce(Seq(mod))(Backend.onlyTFFTest)
//
//    assert(res.head == modres)
//    assert(InlineEverythingOnce.removableVars contains MetaVar("x"))
//    assert(InlineEverythingOnce.removableVars contains MetaVar("y"))
//    assert(InlineEverythingOnce.removableVars.size == 2)
//  }
//
//  //  test("Inlining more complex cycle (do nothing)") {
//  //    val tr = TypingRule("simple",
//  //      Seq(genEq(genMeta("y"), genApp1("f", "x")),
//  //        genEq(genMeta("x"), genMeta("y"))),
//  //      Seq(genEq(genApp1("g", "z"), genMeta("y"))))
//  //    val mod = genSimpleModule("inlinesimple", tr)
//  //
//  //    val trres = TypingRule("simple",
//  //      Seq(genEq(genMeta("y"), genApp1("f", "y")),
//  //        genEq(genMeta("x"), genMeta("y"))),
//  //      Seq(genEq(genApp1("g", "z"), genApp1("f", "y"))))
//  //    val modres = genSimpleModule("inlinesimple", trres)
//  //
//  //    
//  //  }
//
//  test("Inlining with exists") {
//    val tr = TypingRule("simple",
//      Seq(genExists1(List("x", "z"), genEq(genMeta("x"), genMeta("z"))),
//        genEq(genMeta("x"), genMeta("y"))),
//      Seq(genEq(genApp1("f", "x"), genMeta("r"))))
//    val mod = genSimpleModule("inlinesimple", tr)
//
//    val trres = TypingRule("simple",
//      Seq(genExists1(List("x0", "z0"), genEq(genMeta("x0"), genMeta("z0"))),
//        genEq(genMeta("x"), genMeta("y"))),
//      Seq(genEq(genApp1("f", "y"), genMeta("r"))))
//    val modres = genSimpleModule("inlinesimple", trres)
//
//    val res = InlineEverythingOnce(Seq(mod))(Backend.onlyTFFTest)
//
//    assert(res.head == modres)
//    assert(InlineEverythingOnce.removableVars contains MetaVar("x"))
//    assert(InlineEverythingOnce.removableVars.size == 1)
//
//  }
//
//  test("Inlining with forall") {
//    val tr = TypingRule("simple",
//      Seq(genForall1(List("x", "z"), genEq(genMeta("x"), genMeta("z"))),
//        genEq(genMeta("x"), genMeta("y"))),
//      Seq(genEq(genApp1("f", "x"), genMeta("r"))))
//    val mod = genSimpleModule("inlinesimple", tr)
//
//    val trres = TypingRule("simple",
//      Seq(genForall1(List("x0", "z0"), genEq(genMeta("x0"), genMeta("z0"))),
//        genEq(genMeta("x"), genMeta("y"))),
//      Seq(genEq(genApp1("f", "y"), genMeta("r"))))
//    val modres = genSimpleModule("inlinesimple", trres)
//
//    val res = InlineEverythingOnce(Seq(mod))(Backend.onlyTFFTest)
//
//    assert(res.head == modres)
//    assert(InlineEverythingOnce.removableVars contains MetaVar("x"))
//    assert(InlineEverythingOnce.removableVars.size == 1)
//  }
//
//  test("") {
//
//  }
  
}