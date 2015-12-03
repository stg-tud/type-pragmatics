package de.tu_darmstadt

import org.scalatest.FunSuite
import java.io.PrintWriter
import java.io.StringWriter
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.util.prettyprint.SimplePrettyPrintable
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.transformation.SplitModulesByGoal
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.Backend
import de.tu_darmstadt.veritas.backend.veritas.function.FunctionExpTrue

class SplitGoalTest extends FunSuite {

  test("No single goal duplication") {
    val mod = Module("test", Seq(), Seq(Goals(
      Seq(TypingRule("test1", Seq(), Seq(FunctionExpJudgment(FunctionExpTrue)))), None)))

    val res = SplitModulesByGoal()(Seq(mod))(Backend.onlyTFFTest)

    assert(res.length == 1)
  }

  test("No goal in local duplication") {
    val mod = Module("test", Seq(), Seq(
        Local(Seq(
        Goals(
      Seq(TypingRule("test1", Seq(), Seq(FunctionExpJudgment(FunctionExpTrue)))), None)))))

    val res = SplitModulesByGoal()(Seq(mod))(Backend.onlyTFFTest)

    assert(res.length == 1)
  }

}
