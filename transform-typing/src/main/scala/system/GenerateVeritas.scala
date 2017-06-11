package system

import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.fof
import de.tu_darmstadt.veritas.backend.fof.Parenthesized
import system.GenerateTFF.{compilePropTerm, compileVar}
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

  def compilePropTermToVeritas(t: Term): TypingRuleJudgment = t match {
    case t: App => compileJudgToVeritas(t.toJudg)
  }

  def compileJudgToVeritas(judg: Judg): TypingRuleJudgment = judg.sym match {
    case sym if sym.isEq => FunctionExpJudgment(FunctionExpEq(compileTermToVeritas(judg.terms(0)), compileTermToVeritas(judg.terms(1))))
    case sym if sym.isNeq => FunctionExpJudgment(FunctionExpNeq(compileTermToVeritas(judg.terms(0)), compileTermToVeritas(judg.terms(1))))
    case AND => OrJudgment(Seq(Seq(compilePropTermToVeritas(judg.terms(0)), compilePropTermToVeritas(judg.terms(1)))))
    case OR => OrJudgment(Seq(Seq(compilePropTermToVeritas(judg.terms(0))), Seq(compilePropTermToVeritas(judg.terms(1)))))
    case NOT => NotJudgment(compilePropTermToVeritas(judg.terms(0)))
    case sym => FunctionExpJudgment(FunctionExpApp(sym.name, judg.terms.map(compileTermToVeritas(_))))
  }

  def compileRuleToVeritas(rule: Rule): TypingRule =
    TypingRule(rule.name, rule.premises.map(compileJudgToVeritas(_)), Seq(compileJudgToVeritas(rule.conclusion)))


}