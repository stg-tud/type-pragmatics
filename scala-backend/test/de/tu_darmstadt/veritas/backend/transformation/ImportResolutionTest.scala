package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.transformation.imports.ResolveImports
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpJudgment
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpTrue
import de.tu_darmstadt.veritas.backend.veritas.TypingRule
import de.tu_darmstadt.veritas.backend.Backend
import de.tu_darmstadt.veritas.backend.veritas.Goals
import org.scalatest.FunSuite
import de.tu_darmstadt.veritas.backend.veritas.Module

class ImportResolutionTest extends FunSuite {

  def genTrueRule(name: String) = TypingRule(name, Seq(), Seq(FunctionExpJudgment(FunctionExpTrue)))

  test("No goal duplication during import resolve") {
    val mod = Module("test", Seq(), Seq(Goals(Seq(genTrueRule("test")), None)))

    val res = ResolveImports(Seq(mod))(Backend.onlyTFFTest)

    assert(res.head.defs.length == 1)
  }
  
}