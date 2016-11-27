package system

import de.tu_darmstadt.veritas.backend.ast.{DataTypeConstructor, SortRef}
import de.tu_darmstadt.veritas.backend.fof
import de.tu_darmstadt.veritas.backend.fof.{Term => _, _}
import de.tu_darmstadt.veritas.backend.tff._
import de.tu_darmstadt.veritas.backend.transformation.ToTff
import de.tu_darmstadt.veritas.backend.transformation.defs.GenerateCtorAxiomsTyped
import system.Syntax._

object GenerateTFF {

  type Types = Map[ISort, TffAtomicType]

  def compileSymbol(sym: Symbol, types: Types): TypedSymbol = {
    val t = TffMappingType(sym.in.map(types(_)), types(sym.out))
    TypedSymbol(sym.name, t)
  }

  def compileVar(v: Var, types: Types): TypedVariable =
    TypedVariable(v.name, types(v.sort))

  def compileTerm(t: Term, types: Types): fof.Term = t match {
    case v: Var => compileVar(v, types)
    case App(sym, kids) => Appl(compileSymbol(sym, types), kids.map(compileTerm(_, types)))
  }

  def compileJudg(judg: Judg, types: Types): FofUnitary = {
    val kids = judg.terms.map(compileTerm(_, types))
    Appl(compileSymbol(judg.sym, types), kids)
  }

  def compileRule(rule: Rule, types: Types): (String, FofUnitary) = {
    val name = rule.name
    val pre = Parenthesized(And(rule.premises.map(compileJudg(_, types))))
    val con = compileJudg(rule.conclusion, types)
    val body = Parenthesized(Impl(pre, con))
    val vars = rule.freevars
    if (vars.isEmpty)
      (name, body)
    else {
      val allvars = vars.toList.map(compileVar(_, types))
      val all = ForAll(allvars, body)
      (name, all)
    }
  }


  def compileLanguage(lang: Language): Unit = {

  }

  def compileClosedDataType(sort: Sort, constrs: Set[Symbol]): Seq[TffAnnotated] = {
    val dataConstrs = constrs.toSeq.map(c => DataTypeConstructor(c.name, c.in.map(s => SortRef(s.name))))

    val domTR = GenerateCtorAxiomsTyped.makeDomainAxiom(sort.name, dataConstrs)
    val eqTRs = dataConstrs.map(dc => GenerateCtorAxiomsTyped.makeEqAxiom(dc))
    val diffTRs = GenerateCtorAxiomsTyped.makeDiffAxioms(dataConstrs)

    (new ToTff).translateAxioms(domTR +: (eqTRs ++ diffTRs))
  }
}