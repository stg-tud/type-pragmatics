package system

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionDef, FunctionSig}
import de.tu_darmstadt.veritas.backend.ast.{DataTypeConstructor, Functions, SortRef, TypingRule}
import de.tu_darmstadt.veritas.backend.fof
import de.tu_darmstadt.veritas.backend.fof.{Term => _, _}
import de.tu_darmstadt.veritas.backend.tff._
import de.tu_darmstadt.veritas.backend.transformation.ToTff
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectTypesClass
import de.tu_darmstadt.veritas.backend.transformation.defs.GenerateCtorAxiomsTyped
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import system.Syntax._

object GenerateTFF {

  def compileSort(s: ISort): TffAtomicType = s match {
    case Prop => DefinedType("o")
    case _ => SymbolType(TypedSymbol(s.name, DefinedType("tType")))
  }

  def compileSortRef(s: ISort): SortRef = SortRef(s.name)

  def compileSymbol(sym: Symbol): TypedSymbol =
    if (sym.in.isEmpty)
      TypedSymbol(sym.name, compileSort(sym.out))
    else {
      val t = TffMappingType(sym.in.map(compileSort(_)), compileSort(sym.out))
      TypedSymbol(sym.name, t)
    }

  def compileSymbolDeclaration(sym: Symbol): TffAnnotated =
    TffAnnotated(sym.name + "_type", Type, compileSymbol(sym))

  def compileVar(v: Var): Variable =
    UntypedVariable(v.name)

  def compileTerm(t: Term): fof.Term = t match {
    case v: Var => compileVar(v)
    case App(sym, kids) => Appl(UntypedFunSymbol(sym.name), kids.map(compileTerm(_)))
  }

  def compileJudg(judg: Judg): FofUnitary = {
    val kids = judg.terms.map(compileTerm(_))
    Appl(UntypedFunSymbol(judg.sym.name), kids)
  }

  def compileRule(rule: Rule): (String, FofUnitary) = {
    val name = rule.name
    val pre = Parenthesized(And(rule.premises.map(compileJudg(_))))
    val con = compileJudg(rule.conclusion)
    val body = Parenthesized(Impl(pre, con))
    val vars = rule.freevars
    if (vars.isEmpty)
      (name, body)
    else {
      val allvars = vars.toList.map(compileVar(_))
      val all = ForAll(allvars, body)
      (name, all)
    }
  }

  def compileOpenDataType(sort: ISort, toTFF: ToTff): Seq[TffAnnotated] = {
    val typeDecl = TffAnnotated(sort.name + "_type", Type, toTFF.makeTopLevelSymbol(sort.name))

    val initName = sort.name + "_init"
    val enumName = sort.name + "_enum"
    val initSym = Symbol(initName, in = List(), out = sort)
    val enumSym = Symbol(enumName, in = List(sort), out = sort)
    val funDecls = Seq(compileSymbolDeclaration(initSym), compileSymbolDeclaration(enumSym))

    val initConstr = DataTypeConstructor(initName, Seq())
    val enumConstr = DataTypeConstructor(enumName, Seq(compileSortRef(sort)))
    val enumEq = GenerateCtorAxiomsTyped.makeEqAxiom(enumConstr)
    val diff = GenerateCtorAxiomsTyped.makeDiffAxioms(Seq(initConstr, enumConstr))
    val axioms = toTFF.translateAxioms(enumEq +: diff)

    typeDecl +: (funDecls ++ axioms)
  }

  def compileClosedDataType(sort: Sort, constrs: Seq[Symbol], toTFF: ToTff): Seq[TffAnnotated] = {
    val typeDecl = TffAnnotated(sort.name + "_type", Type, toTFF.makeTopLevelSymbol(sort.name))
    val constrDecls = constrs.map(compileSymbolDeclaration(_))

    val dataConstrs = constrs.map(c => DataTypeConstructor(c.name, c.in.map(s => compileSortRef(s))))
    val domTR = GenerateCtorAxiomsTyped.makeDomainAxiom(sort.name, dataConstrs)
    val eqTRs = dataConstrs.map(dc => GenerateCtorAxiomsTyped.makeEqAxiom(dc))
    val diffTRs = GenerateCtorAxiomsTyped.makeDiffAxioms(dataConstrs)
    val axioms = toTFF.translateAxioms(domTR +: (eqTRs ++ diffTRs))

    typeDecl +: (constrDecls ++ axioms)
  }

  def compileLanguage(lang: Language): Seq[TffAnnotated] = {
    val toTFF = makeToTFF(lang)

    val open = lang.openDataTypes.flatMap(compileOpenDataType(_, toTFF))
    val closed = lang.closedDataTypes.flatMap { case (sort, constrs) => compileClosedDataType(sort, constrs, toTFF) }
    val funs = lang.funSymbols.map(compileSymbolDeclaration(_))
    val rules = lang.rules.map { rule =>
      val (name, body) = compileRule(rule)
      TffAnnotated(name, Axiom, body)
    }

    open ++ closed ++ funs ++ rules
  }

  class LanguageCollectTypes(lang: Language) extends CollectTypesClass {
    private val syms: Map[String, Symbol] = {
      val syms = lang.syms.map(sym => sym.name -> sym).toMap
      val opens = lang.openDataTypes.flatMap(s => Seq(Symbol(s + "_init", List(), s), Symbol(s + "_enum", List(s), s)))
      syms ++ opens.map(s => s.name -> s).toMap
    }

    override def symbolType(name: String): Option[(Seq[SortRef], SortRef)] = syms.get(name) match {
      case None => super.symbolType(name)
      case Some(sym) => Some((sym.in.map(compileSortRef(_)), compileSortRef(sym.out)))
    }
  }

  def makeToTFF(lang: Language): ToTff = {
    val types = new LanguageCollectTypes(lang)
    val toTff = new ToTff
    toTff.setTypes(types)
    for (s <- lang.sorts)
      toTff.addTSIfNew(toTff.makeTopLevelSymbol(s.name))
    toTff
  }
}