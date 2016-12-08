package system

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.fof
import de.tu_darmstadt.veritas.backend.fof.{Term => _, _}
import de.tu_darmstadt.veritas.backend.tff._
import de.tu_darmstadt.veritas.backend.transformation.ToTff
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectTypesClass
import de.tu_darmstadt.veritas.backend.transformation.defs.{AllFunctionInversionAxioms, CurrentFun, GenerateCtorAxiomsTyped}
import system.Syntax._
import system.GenerateVeritas._

object GenerateTFF {

  def compileSort(s: ISort): TffAtomicType = s match {
    case Prop => DefinedType("o")
    case _ => SymbolType(TypedSymbol(s.name, DefinedType("tType")))
  }

  def compileSortDecl(sort: ISort): TffAnnotated =
    TffAnnotated(sort.name + "_type", Type, TypedSymbol(sort.name, DefinedType("tType")))


  def compileSymbol(sym: Symbol): TypedSymbol =
    if (sym.in.isEmpty)
      TypedSymbol(sym.name, compileSort(sym.out))
    else {
      val t = TffMappingType(sym.in.map(compileSort(_)), compileSort(sym.out))
      TypedSymbol(sym.name, t)
    }

  def compileSymbolDeclaration(sym: Symbol): TffAnnotated =
    TffAnnotated(sym.name + "_type", Type, compileSymbol(sym))

  def compileVar(v: Var, typed: Boolean = false): Variable =
    if (typed)
      TypedVariable(v.name, compileSort(v.sort))
    else
      UntypedVariable(v.name)

  def compileTerm(t: Term): fof.Term = t match {
    case v: Var => compileVar(v)
    case App(sym, kids) => Appl(UntypedFunSymbol(sym.name), kids.map(compileTerm(_)))
  }

  def compileJudg(judg: Judg): FofUnitary = judg.sym match {
    case sym if sym.isEq => fof.Eq(compileTerm(judg.terms(0)), compileTerm(judg.terms(1)))
    case sym if sym.isNeq => fof.NeqEq(compileTerm(judg.terms(0)), compileTerm(judg.terms(1)))
    case _ =>
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
      val allvars = vars.toList.map(compileVar(_, typed = true))
      val all = ForAll(allvars, body)
      (name, all)
    }
  }

  def compileRuleDecl(rule: Rule): TffAnnotated = {
    val (name, body) = compileRule(rule)
    TffAnnotated(name, Axiom, body)
  }

  def compileOpenDataType(sort: ISort, toTFF: ToTff): Seq[TffAnnotated] = {
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

    funDecls ++ axioms
  }

  def compileClosedDataType(sort: Sort, constrs: Seq[Symbol], toTFF: ToTff): Seq[TffAnnotated] = {
    val constrDecls = constrs.map(compileSymbolDeclaration(_))

    val dataConstrs = constrs.map(c => DataTypeConstructor(c.name, c.in.map(s => compileSortRef(s))))
    val domTR = GenerateCtorAxiomsTyped.makeDomainAxiom(sort.name, dataConstrs)
    val eqTRs = dataConstrs.map(dc => GenerateCtorAxiomsTyped.makeEqAxiom(dc))
    val diffTRs = GenerateCtorAxiomsTyped.makeDiffAxioms(dataConstrs)
    val axioms = toTFF.translateAxioms(domTR +: (eqTRs ++ diffTRs))

    constrDecls ++ axioms
  }

  def compileImplicitSymbols(syms: Set[Symbol]): Seq[TffAnnotated] = {
    syms.toSeq.flatMap {
      case sym if sym.isFresh =>
        val sort = sym.in(0)
        val freshSym = Names.fresh(sort)
        val freshIsNotin = Names.freshNotin(sort)
        // we assume symbol `notinSym` is already declared as part of the language
        Seq(compileSymbolDeclaration(freshSym), compileRuleDecl(freshIsNotin))
      case sym if sym.isNotin =>
        Seq(compileSymbolDeclaration(sym))
      case _ =>
        // ignore isEq and isNeq since they translate to TFF-native `=` and `!=`
        None
    }
  }

  def compileLanguage(lang: Language): Seq[TffAnnotated] = {
    val toTFF = makeToTFF(lang)

    val types = lang.sorts.map(compileSortDecl(_))
    val open = lang.openDataTypes.flatMap(compileOpenDataType(_, toTFF))
    val closed = lang.closedDataTypes.flatMap { case (sort, constrs) => compileClosedDataType(sort, constrs, toTFF) }
    val funs = lang.funSymbols.map(compileSymbolDeclaration(_))
    val implicits = compileImplicitSymbols(lang.undeclaredSymbols)

    val rules = lang.rules.map(compileRuleDecl(_))
    val groupedRules = lang.rules.filter(!_.lemma).groupBy(_.conclusion.sym)
    val inversionRules = groupedRules.flatMap(r => compileInversionRule(r._1, r._2, toTFF))

    val transs = lang.transs.flatMap(compileTransformation(_))

    types ++ open ++ closed ++ funs ++ implicits ++ rules ++ inversionRules ++ transs
  }

  def compileTransformation(trans: Transformation, withContract: Boolean = true): Seq[TffAnnotated] = {
    val implicits = compileImplicitSymbols(trans.undeclaredSymbols)
    val sym = compileSymbolDeclaration(trans.contractedSym)
    val contracts = if (withContract) trans.contracts.keys.map(compileRuleDecl(_)) else Seq()
    val rewrites = compileRewrites(trans.rewrites)
    implicits ++ Seq(sym) ++ contracts ++ rewrites
  }



  def compileRewrites(rewrites: Seq[Rewrite]): Seq[TffAnnotated] = {
    // TODO generate rewrite rules
    // TODO   linear patterns -> function
    // TODO   otherwise -> relation
    // TODO generate inversion rule

    val sym = rewrites.head.pat.sym

    rewrites.zipWithIndex.map { case (r, i) =>
      val premises = r.where
      val conclusion = Judg(equ(r.sym.out), r.pat, r.gen)
      compileRuleDecl(Rule(s"${r.sym.name}-$i", conclusion, premises.toList))
    }
  }


  def compileInversionRule(sym: Symbol, rules: Seq[Rule], toTFF: ToTff): Seq[TffAnnotated] = {
    val gensym = new Gensym
    val inputVars = sym.in.map(s => gensym.freshVar(s.name, s))
    val outputVar = gensym.freshVar(sym.out.name, sym.out)
    val fun = CurrentFun(
      sym.name,
      sym.in.map(compileSortRef(_)),
      compileSortRef(sym.out),
      inputVars.map(v => MetaVar(v.name)),
      MetaVar(outputVar.name)
    )

    val tseq = rules.map(compileRuleToVeritas(_))
    val axioms = AllFunctionInversionAxioms.generateInversionAxiom(fun, tseq)
    toTFF.translateAxioms(axioms.axioms)
  }



  class LanguageCollectTypes(lang: Language) extends CollectTypesClass {
    private val syms: Map[String, Symbol] = {
      val syms = lang.syms.map(sym => sym.name -> sym).toMap
      val openSyms = lang.openDataTypes.flatMap(s => Seq(Symbol(s + "_init", List(), s), Symbol(s + "_enum", List(s), s)))
      val opens = openSyms.map(s => s.name -> s).toMap
      val undeclared = lang.undeclaredSymbols.map(s => s.name -> s).toMap
      syms ++ opens ++ undeclared
    }

    override def symbolType(name: String): Option[(Seq[SortRef], SortRef)] = syms.get(name) match {
      case None => super.symbolType(name)
      case Some(sym) => Some((sym.in.map(compileSortRef(_)), compileSortRef(sym.out)))
    }
  }

  private def makeToTFF(lang: Language): ToTff = {
    val types = new LanguageCollectTypes(lang)
    val toTff = new ToTff
    toTff.setTypes(types)
    for (s <- lang.sorts)
      toTff.addTSIfNew(toTff.makeTopLevelSymbol(s.name))
    toTff
  }
}