package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.fof._
import de.tu_darmstadt.veritas.backend.tff._
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.util.FreeVariables

/**
 * Transforms Core TSSL (Veritas) Modules to TFF syntax
 *
 * Structure of Core Modules
 * - no imports
 * - section with "symbol declarations" (constructor decls, const decls, function sigs...) (can be empty)
 * - section with n axioms, where typing judgments were already transformed to some typed function! (can be empty)
 * - exactly one goal! (which must not be followed by other axioms, constructors etc, which would be out of scope!)
 */
class CollectTypes extends ModuleTransformation {

  /**
   * collects user-defined atomic types
   * (sorts and simple types, already translated to typed symbols)
   */
  private var _typedSymbols: Map[String, TypedSymbol] = Map()
  def typedSymbols = _typedSymbols

  /**
   * top-level function for translating a Module to a TffFile
   */
  override def transModule(name: String, is: Seq[Import], mdefs: Seq[ModuleDef]): Seq[Module] = {
    _typedSymbols = Map()
    mdefs foreach {
      case d: DataType => addDataType(d)
      case _ =>
    }
    super.transModule(name, is, mdefs)
  }

  /**
   * Make sure that type symbols are properly scoped by local and strategy blocks
   */
  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = mdef match {
      case Local(_) | Strategy(_,_,_) => 
        val oldTypedSymbols = _typedSymbols
        val res = super.transModuleDefs(mdef)
        _typedSymbols = oldTypedSymbols
        res
      case _ => 
        super.transModuleDefs(mdef)
    }

  /**
   * collects newly discovered data types,
   * adds top-level type declaration to typedecllist
   *
   * throws an error if the data type is already defined
   */
  private def addDataType(d: DataType): DataType = {
    // note: data types can never contain predefined sorts (ensured via "require")
    // so blindly adding the DataType is ok
    val ts = makeTopLevelSymbol(d.name)
    addTSIfNew(ts)
    d
  }

  override def transDataTypeConstructor(d: DataTypeConstructor, open: Boolean, dataType: String): Seq[DataTypeConstructor] = {
    withSuper(super.transDataTypeConstructor(d, open, dataType)) {
      case d =>
        val outt = makeAtomicType(dataType)
        val ints = d.in map (s => makeAtomicType(s.name))
        val t = if (ints.isEmpty) outt else
          TffMappingType(ints, outt)
        val ts = TypedSymbol(d.name, t)
        addTSIfNew(ts)
        Seq(d)
    }
  }

  override def transConstDecl(d: ConstDecl): Seq[ConstDecl] = {
    withSuper(super.transConstDecl(d)) {
      case d =>
        val outt = makeAtomicType(d.out.name)
        val ts = TypedSymbol(d.name, outt)
        addTSIfNew(ts)
        Seq(d)
    }
  }

  override def transSortDefs(sd: SortDef): Seq[SortDef] =
    withSuper(super.transSortDefs(sd)) {
      case sd =>
        val ts = makeTopLevelSymbol(sd.name)
        addTSIfNew(ts)
        Seq(sd)
    }

  override def transFunctionSig(sig: FunctionSig): FunctionSig =
    withSuper(super.transFunctionSig(sig)) {
      case sig =>
        val outt = makeAtomicType(sig.out.name)
        val ints = sig.in map (s => makeAtomicType(s.name))
        val t = if (ints.isEmpty) outt else TffMappingType(ints, outt)
        val ts = TypedSymbol(sig.name, t)
        addTSIfNew(ts)
        sig
    }

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] =
    withSuper(super.transTypingRules(tr)) {
      case tr =>
        Seq(tr)
    }

  /**
   * create a top-level typed symbol
   */
  private def makeTopLevelSymbol(name: String): TypedSymbol =
    TypedSymbol(name, DefinedType("tType"))

  private def addTSIfNew(ts: TypedSymbol): Unit =
    if (_typedSymbols.contains(ts.name))
      throw TransformationError(s"Sort, Constructor, or function ${ts.name} has been defined twice!")
    else
      _typedSymbols += ts.name -> ts

  private def translatePredefinedType(t: String): DefinedType =
    t match {
      case "Bool" => DefinedType("o")
      case "iType" => DefinedType("i")
      case n => DefinedType(n)
    }

  /**
   * transforms a SortRef into a type for Tff
   * Case 1) predefined type, returns a DefinedType
   * Case 2) type is not predefined - checks whether type was already defined and throws an error if not!
   * otherwise returns a SymbolType
   *
   */
  def makeAtomicType(name: String): TffAtomicType =
    if (SortDef.predefinedSorts contains name)
      translatePredefinedType(name)
    else {
      val ts = makeTopLevelSymbol(name)
      _typedSymbols.get(name) match {
        case Some(ts) => SymbolType(ts)
        case None => throw TransformationError(s"Encountered sort reference ${name}, which has not been defined yet!")
      }
    }

  def inferMetavarTypes(tr: TypingRule): Map[MetaVar, TffAtomicType] = {
    val jdgs = tr.premises ++ tr.consequences
    val vars = FreeVariables.freeVariables(jdgs)
    inferMetavarTypes(vars, jdgs)
  }

  def inferMetavarTypes(vars: Iterable[MetaVar], jdgs: Seq[TypingRuleJudgment]): Map[MetaVar, TffAtomicType] = {
    Map() ++ (for (v <- vars) yield {
      val occurrences = findTypableOccurrences(v, jdgs)
      val typelist = occurrences flatMap (typeOcc(v, _))
      val typeset = typelist.toSet //remove duplicate occurrences of found types; ideally, only one occurrence should be left!
      typeset.size match {
        case 0 => throw TransformationError(s"Could not infer type of metavariable ${v.name}, no candidate found.") //has to be an error case, untyped variables will not be supported by e.g. Vampire!
        case 1 => (v -> typeset.head)
        case _ => throw TransformationError(s"Could not infer type of metavariable ${v.name}, multiple candidates found $typeset")
      }
    })
  }

  /**
   * finds constructs in given sequence of TypingRuleJudgment that have the
   * given MetaVar m as direct child and are one of
   * - function applications (Appl)
   * - equalities/inequalities with the m on one side and a function application on the other side
   *
   */
  private def findTypableOccurrences(m: MetaVar, jdglist: Seq[TypingRuleJudgment]): Set[FunctionExp] = {
    def containsMetaVar(args: Seq[FunctionExpMeta]): Boolean = args exists
      {
        case FunctionMeta(m0) => m == m0
        case _ => false
      }

    def searchFunctionExp(e: FunctionExpMeta): Set[FunctionExp] = {
      e match {
        // base cases just return empty set, all listed explicitly just in case!  
        case FunctionMeta(m) => Set()
        case FunctionExpVar(n) => Set()
        case FunctionExpTrue => Set()
        case FunctionExpFalse => Set()
        case FunctionExpNot(f) => searchFunctionExp(f)
        case fe @ FunctionExpEq(FunctionMeta(mx), r @ FunctionExpApp(n, args)) =>
          if (mx == m) Set(fe) else searchFunctionExp(r)
        case fe @ FunctionExpEq(l @ FunctionExpApp(n, args), r @ FunctionMeta(mx)) =>
          if (mx == m) Set(FunctionExpEq(r, l)) else searchFunctionExp(l)
        case FunctionExpEq(f1, f2) => searchFunctionExp(f1) ++ searchFunctionExp(f2)
        case fe @ FunctionExpNeq(FunctionMeta(mx), r @ FunctionExpApp(n, args)) =>
          if (mx == m) Set(fe) else searchFunctionExp(r)
        case fe @ FunctionExpNeq(l @ FunctionExpApp(n, args), r @ FunctionMeta(mx)) =>
          if (mx == m) Set(FunctionExpNeq(r, l)) else searchFunctionExp(l)
        case FunctionExpNeq(f1, f2) => searchFunctionExp(f1) ++ searchFunctionExp(f2)
        case FunctionExpAnd(l, r) => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpOr(l, r) => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpBiImpl(l, r) => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpIf(c, t, e) => searchFunctionExp(c) ++ searchFunctionExp(t) ++ searchFunctionExp(e)
        case FunctionExpLet(n, e, i) => searchFunctionExp(e) ++ searchFunctionExp(i)
        case fe @ FunctionExpApp(fn, args) => {
          val afe = (args flatMap searchFunctionExp).toSet
          if (containsMetaVar(args)) afe ++ Set(fe) else afe
        }
        // this should never happen
        case _ => throw TransformationError(s"While trying to type meta variable ${m.name}, found a function expression $e that is not covered by the code!")
      }
    }
    (for (jdg <- jdglist) yield {
      jdg match {
        case FunctionExpJudgment(f) => searchFunctionExp(f)
        case ExistsJudgment(vl, jdgl) => findTypableOccurrences(m, jdgl)
        case ForallJudgment(vl, jdgl) => findTypableOccurrences(m, jdgl)
        case NotJudgment(jdg) => findTypableOccurrences(m, Seq(jdg))
        case OrJudgment(jdgll) => jdgll flatMap (findTypableOccurrences(m, _))
        case _ => throw TransformationError(s"While trying to type meta variable ${m.name}, encountered a construct in an axiom, premise, or conclusion that is not supported by core modules!")
      }
    }).flatten.toSet
  }

  /**
   * given a FunctionExp such as the ones found by findTypableOccurrences,
   * tries to determine the type of the given MetaVar m
   */
  private def typeOcc(m: MetaVar, occ: FunctionExp): Option[TffAtomicType] = {

    def getMetaVarPos(args: Seq[FunctionExpMeta]): Seq[Int] =
      for (i <- args.indices if (args(i) == FunctionMeta(m))) yield i

    def getReturnType(t: TffTopLevelType): Option[TffAtomicType] =
      t match {
        case TffMappingType(_, restype) => Some(restype)
        case st @ SymbolType(t) => Some(st)
        case _ => None
      }

    def getArgType(t: TffTopLevelType, args: Seq[FunctionExpMeta]): Option[TffAtomicType] =
      t match {
        case TffMappingType(arglist, _) => {
          val poslist = getMetaVarPos(args)
          val typeset = (for (p <- poslist) yield arglist(p)).toSet
          if (typeset.size == 1) Some(typeset.head) else None
        }
        case _ => None
      }

    occ match {
      case FunctionExpEq(FunctionMeta(mx @ _), FunctionExpApp(n, _)) if (mx == m) => getReturnType(_typedSymbols(n).tfftype)
      case FunctionExpNeq(FunctionMeta(mx @ _), FunctionExpApp(n, _)) if (mx == m) => getReturnType(_typedSymbols(n).tfftype)
      case FunctionExpApp(fn, args) => getArgType(_typedSymbols(fn).tfftype, args)
      case _ => throw TransformationError(s"While trying to type meta variable ${m.name}, an untypable FunctionExp was marked as typable.")
    }
  }
}
