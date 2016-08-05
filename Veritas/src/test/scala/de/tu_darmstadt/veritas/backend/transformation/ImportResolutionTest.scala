package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.transformation.imports.ResolveImports
import de.tu_darmstadt.veritas.backend.ast.FunctionExpJudgment
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpTrue
import de.tu_darmstadt.veritas.backend.ast.TypingRule
import de.tu_darmstadt.veritas.backend.Backend
import de.tu_darmstadt.veritas.backend.ast.Goals
import org.scalatest.FunSuite
import de.tu_darmstadt.veritas.backend.ast.Module
import de.tu_darmstadt.veritas.backend.ast.function.FunctionSig
import de.tu_darmstadt.veritas.backend.ast.SortRef
import de.tu_darmstadt.veritas.backend.ast.MetaVar
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpApp
import de.tu_darmstadt.veritas.backend.ast.function.FunctionMeta
import de.tu_darmstadt.veritas.backend.ast.function.FunctionDef
import de.tu_darmstadt.veritas.backend.ast.Resolved
import de.tu_darmstadt.veritas.backend.ast.ModuleDef
import de.tu_darmstadt.veritas.backend.ast.Functions
import de.tu_darmstadt.veritas.backend.ast.Local

class ImportResolutionTest extends FunSuite {

  def genTrueRule(name: String) = TypingRule(name, Seq(), Seq(FunctionExpJudgment(FunctionExpTrue)))

  def genTestFunctionSig(name: String) = Functions(Seq(FunctionDef(FunctionSig(name, Seq(SortRef("A")), SortRef("B")), Seq())))

  def genFunctionApp(name: String, mv: MetaVar) = FunctionExpApp(name, Seq(FunctionMeta(mv)))

  test("No goal duplication during import resolve") {
    val mod = Module("test", Seq(), Seq(Goals(Seq(genTrueRule("test")), None)))

    val res = ResolveImports(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head.defs.length == 1)
  }

  test("Correct simple Import") {
    val impdef = genTestFunctionSig("f")
    val imp = Resolved(Module("ImportMe", Seq(), Seq(impdef)))

    val mod = Module("test", Seq(imp), Seq(Goals(Seq(genTrueRule("test")), None)))

    val resmod = Module("test", Seq(), Seq(impdef, Goals(Seq(genTrueRule("test")), None)))

    val res = ResolveImports(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == resmod)

  }

  test("No import of goals") {
    val impdef = genTestFunctionSig("f")
    val imp = Resolved(Module("ImportMe", Seq(), Seq(impdef, Goals(Seq(genTrueRule("donotimportme")), None))))

    val mod = Module("test", Seq(imp), Seq(Goals(Seq(genTrueRule("test")), None)))

    val resmod = Module("test", Seq(), Seq(impdef, Goals(Seq(genTrueRule("test")), None)))

    val res = ResolveImports(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == resmod)

  }
  
  
   test("No import of locals") {
    val impdef = genTestFunctionSig("f")
    val imp = Resolved(Module("ImportMe", Seq(), Seq(impdef, Local(Seq(Goals(Seq(genTrueRule("donotimportme")), None))))))

    val mod = Module("test", Seq(imp), Seq(Goals(Seq(genTrueRule("test")), None)))

    val resmod = Module("test", Seq(), Seq(impdef, Goals(Seq(genTrueRule("test")), None)))

    val res = ResolveImports(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == resmod)

  }

  test("Correct multiple simple Import (does not modify imp-order)") {
    val impdef1 = genTestFunctionSig("f1")
    val imp1 = Resolved(Module("ImportMe1", Seq(), Seq(impdef1)))

    val impdef2 = genTestFunctionSig("f2")
    val imp2 = Resolved(Module("ImportMe2", Seq(), Seq(impdef2)))

    val impdef3 = genTestFunctionSig("f3")
    val imp3 = Resolved(Module("ImportMe3", Seq(), Seq(impdef3)))

    val mod = Module("test", Seq(imp1, imp2, imp3), Seq(Goals(Seq(genTrueRule("test")), None)))

    val resmod = Module("test", Seq(), Seq(impdef1, impdef2, impdef3, Goals(Seq(genTrueRule("test")), None)))

    val res = ResolveImports(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == resmod)
  }
  
  test("Correct Import simple dependencies") {
    val impdef1 = genTestFunctionSig("f1")
    val imp1 = Resolved(Module("ImportMe1", Seq(), Seq(impdef1)))

    val impdef2 = genTestFunctionSig("f2")
    val imp2 = Resolved(Module("ImportMe2", Seq(imp1), Seq(impdef2)))

    val impdef3 = genTestFunctionSig("f3")
    val imp3 = Resolved(Module("ImportMe3", Seq(imp2), Seq(impdef3)))

    val mod = Module("test", Seq(imp3), Seq(Goals(Seq(genTrueRule("test")), None)))

    val resmod = Module("test", Seq(), Seq(impdef1, impdef2, impdef3, Goals(Seq(genTrueRule("test")), None)))

    val res = ResolveImports(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == resmod)
  }
  
  test("Correct Import simple dependencies - no module duplication") {
    val impdef1 = genTestFunctionSig("f1")
    val imp1 = Resolved(Module("ImportMe1", Seq(), Seq(impdef1)))

    val impdef2 = genTestFunctionSig("f2")
    val imp2 = Resolved(Module("ImportMe2", Seq(imp1), Seq(impdef2)))

    val impdef3 = genTestFunctionSig("f3")
    val imp3 = Resolved(Module("ImportMe3", Seq(imp1, imp2), Seq(impdef3)))

    val mod = Module("test", Seq(imp3), Seq(Goals(Seq(genTrueRule("test")), None)))

    val resmod = Module("test", Seq(), Seq(impdef1, impdef2, impdef3, Goals(Seq(genTrueRule("test")), None)))

    val res = ResolveImports(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == resmod)
  }
  
  test("Correct Import simple dependencies - no module duplication, arbitrary import order") {
    val impdef1 = genTestFunctionSig("f1")
    val imp1 = Resolved(Module("ImportMe1", Seq(), Seq(impdef1)))

    val impdef2 = genTestFunctionSig("f2")
    val imp2 = Resolved(Module("ImportMe2", Seq(imp1), Seq(impdef2)))

    val impdef3 = genTestFunctionSig("f3")
    val imp3 = Resolved(Module("ImportMe3", Seq(imp2, imp1), Seq(impdef3)))

    val mod = Module("test", Seq(imp3, imp1, imp2), Seq(Goals(Seq(genTrueRule("test")), None)))

    val resmod = Module("test", Seq(), Seq(impdef1, impdef2, impdef3, Goals(Seq(genTrueRule("test")), None)))

    val res = ResolveImports(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head == resmod)
  }
}