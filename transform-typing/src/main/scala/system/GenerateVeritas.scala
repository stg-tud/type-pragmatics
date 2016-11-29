package system

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExpApp, FunctionExpMeta, FunctionMeta}
import de.tu_darmstadt.veritas.backend.ast._
import system.Syntax._

object GenerateVeritas {
  def compileSortRef(s: ISort): SortRef = s match {
    case Prop => SortRef("Bool")
    case _ => SortRef(s.name)
  }

  def compileTermToVeritas(t: Term): FunctionExpMeta = t match {
    case v: Var => FunctionMeta(MetaVar(v.name))
    case App(sym, kids) => FunctionExpApp(sym.name, kids.map(compileTermToVeritas(_)))
  }

  def compileJudgToVeritas(judg: Judg): TypingRuleJudgment =
    FunctionExpJudgment(FunctionExpApp(judg.sym.name, judg.terms.map(compileTermToVeritas(_))))

  def compileRuleToVeritas(rule: Rule): TypingRule = rule match {
    case Rule(name, conc, prems) =>
      TypingRule(name, prems.map(compileJudgToVeritas(_)), Seq(compileJudgToVeritas(conc)))
  }

}