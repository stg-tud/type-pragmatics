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
object ToTff {

  /**
   * list for collecting type declarations from constructor/function declarations in Module
   */
  private var typedecllist: Seq[TffAnnotated] = Seq()

  /**
   * list for collecting the axioms in the module
   */
  private var axiomlist: Seq[TffAnnotated] = Seq()

  /**
   * variable for collecting the goal - if empty at the end, then throw exception!
   */
  private var goal: Option[TffAnnotated] = None

  /**
   * collects user-defined atomic types
   * (sorts and simple types, already translated to typed symbols)
   */
  private var typedSymbols: Set[TypedSymbol] = Set()

  /**
   * top-level function for translating a Module to a TffFile
   */
  def toTffFile(veritasModule: Module): TffFile = {
    //make sure every mutable state is initialized when applying this!
    typedecllist = Seq()
    axiomlist = Seq()
    goal = None
    typedSymbols = Set()
    
    veritasModule match {
      case Module(name, Seq(), body) => {
        try {
          bodyToTff(body)
          constructFinalTff(name + ".tff")
        } catch {
          case TransformationError(s) => throw TransformationError(s"Failed to transform Module ${name} to TFF: " + s)
          case e: Exception           => throw e
        }
      }
      case Module(name, _, _) => throw TransformationError(s"Failed to transform Module ${name} to TFF: Module still contained imports!")
    }
  }

  /**
   *  translate a Module body (sequence of ModuleDefs) to TffAnnotated
   *  adds the collected declarations to the appropriate collections of the object defined above
   */
  private def bodyToTff(body: Seq[ModuleDef]): Unit = {
    body.dropRight(1) foreach { md =>
      md match {
        case Axioms(axs)           => axiomlist ++= translateAxioms(axs)
        case Goals(gs, _)          => throw TransformationError("Found goal in Module which was not at last position!")
        case Constructors(cs)      => addConstDecl(cs)
        case Consts(cs)            => addConstDecl(cs)
        case Sorts(s)              => addSortDef(s)
        case Functions(fds)        => addConstDecl(getFunctionSigs(fds))
        case PartialFunctions(fds) => addConstDecl(getFunctionSigs(fds))
        case _                     => throw TransformationError("Unsupported top-level construct!")

      }
    }
    body.last match {
      case Goals(gs, _) => if (gs.length > 1) throw TransformationError("More than one goal found!")
      else goal = Some(translateGoal(gs(0)))

      case _ => throw TransformationError("Module contained no goal or goal in module was not at last position!")
    }
  }

  /**
   * create a top-level typed symbol
   */
  private def makeTopLevelSymbol(name: String): TypedSymbol =
    TypedSymbol(name, DefinedType("tType"))

  private def addTSIfNew(ts: TypedSymbol): Unit =
    if (typedSymbols contains ts)
      throw TransformationError(s"Sort, Constructor, or function ${ts.name} has been defined twice!")
    else
      typedSymbols += ts

  /**
   * collects newly discovered sorts in sorts variable,
   * adds top-level type declaration to typedecllist
   *
   * throws an error if the sort is already defined
   */
  private def addSortDef(sds: Seq[SortDef]): Unit =
    typedecllist ++=
      (for (SortDef(name) <- sds) yield {
        // note: SortDefs can never contain predefined sorts (ensured via "require")
        // so blindly adding the SortDef is ok
        val ts = makeTopLevelSymbol(name)
        addTSIfNew(ts)
        TffAnnotated(s"${name}_type", Type, ts)
      })

  private def translatePredefinedType(t: String): DefinedType =
    t match {
      case "Bool"  => DefinedType("o")
      case "iType" => DefinedType("i")
      case n       => DefinedType(n)
    }

  /**
   * transforms a SortRef into a type for Tff
   * Case 1) predefined type, returns a DefinedType
   * Case 2) type is not predefined - checks whether type was already defined and throws an error if not!
   * otherwise returns a SymbolType
   *
   */
  private def makeAtomicType(sr: SortRef): TffAtomicType =
    sr match {
      case SortRef(name) =>
        if (SortDef.predefinedSorts contains name)
          translatePredefinedType(name)
        else {
          val ts = makeTopLevelSymbol(name)
          if (typedSymbols contains ts)
            SymbolType(ts)
          else
            throw TransformationError(s"Encountered sort reference ${name}, which has not been defined yet!")
        }
    }

  /**
   * adds a top-level type declaration to typedecllist
   *
   * throws an error if the referenced sorts are not present in sorts set
   */
  private def addConstDecl(cs: Seq[ConstructorDecl]): Unit =
    typedecllist ++=
      (for (ConstructorDecl(name, in, out) <- cs) yield {
        val outt = makeAtomicType(out)
        val ints = in map makeAtomicType
        val t = if (ints.isEmpty) outt else
          TffMappingType(ints, outt)
        val ts = TypedSymbol(name, t)
        addTSIfNew(ts)
        TffAnnotated(s"${name}_type", Type, ts)
      })

  /**
   * extracts function signatures from function definitions, if functions equations are empty
   * transforms function signatures to constructor declarations
   *
   * throws an error if the function equations are not empty (not supported by core modules!)
   */
  private def getFunctionSigs(fds: Seq[FunctionDef]): Seq[ConstructorDecl] =
    for (FunctionDef(FunctionSig(name, in, out), eqs) <- fds) yield if (eqs.isEmpty) ConstructorDecl(name, in, out) else
      throw TransformationError(s"Function definition ${name} still contained untransformed function equations!")

  /**
   * translates axioms to Tff
   */
  private def translateAxioms(axs: Seq[TypingRule]): Seq[TffAnnotated] =
    for (a <- axs)
      yield a match {
      case TypingRule(name, prems, conseqs) =>
        TffAnnotated(name, Axiom, typingRuleToTff(prems, conseqs))
    }

  /**
   * translates goals to Tff
   */
  private def translateGoal(g: TypingRule): TffAnnotated =
    g match {
      case TypingRule(name, prems, conseqs) =>
        TffAnnotated(name, Conjecture, typingRuleToTff(prems, conseqs))
    }

  /**
   * translates typing rules (= implications) to Tff
   * TODO: universal quantification over all free variables!!
   */
  private def typingRuleToTff(prems: Seq[TypingRuleJudgment], conseqs: Seq[TypingRuleJudgment]) = {
    val quantifiedVars = FreeVariables.freeVariables(prems ++ conseqs)
    val transformedprems = prems map jdgtoTff

    if (transformedprems == Seq(True))
      ForAll(makeVarlist(quantifiedVars.toSeq, prems ++ conseqs), Parenthesized(And(conseqs map jdgtoTff)))
    else
      ForAll(makeVarlist(quantifiedVars.toSeq, prems ++ conseqs), Parenthesized(
        (Impl(Parenthesized(And(transformedprems)), Parenthesized(And(conseqs map jdgtoTff))))))
  }

  /**
   * translates individual clauses (premises or conclusion) to Tff (-> FofUnitary)
   */
  private def jdgtoTff(jdg: TypingRuleJudgment): FofUnitary =
    jdg match {
      case FunctionExpJudgment(f)        => functionExpToTff(f)
      case ExistsJudgment(vars, jdglist) => Exists(makeVarlist(vars, jdglist), Parenthesized(And(jdglist map jdgtoTff)))
      case ForallJudgment(vars, jdglist) => ForAll(makeVarlist(vars, jdglist), Parenthesized(And(jdglist map jdgtoTff)))
      case NotJudgment(jdg)              => Not(jdgtoTff(jdg))
      case OrJudgment(ors)               => Parenthesized(Or(ors map (orcase => Parenthesized(And(orcase map jdgtoTff)))))
      case _                             => throw TransformationError("Encountered unsupported judgment while translating a goal or axiom (e.g. typing judgment)")
    }

  /**
   * translate individual function expressions to Tff (-> FofUnitary);
   * outer function expressions cannot be MetaVars, since a MetaVar cannot be translated to a FofUnitary
   */
  private def functionExpToTff(f: FunctionExp): FofUnitary =
    f match {
      case FunctionExpNot(f)       => Not(functionExpToTff(f))
      case FunctionExpEq(f1, f2)   => Eq(functionExpMetaToTff(f1), functionExpMetaToTff(f2))
      case FunctionExpNeq(f1, f2)  => NeqEq(functionExpMetaToTff(f1), functionExpMetaToTff(f2))
      case FunctionExpAnd(l, r)    => Parenthesized(And(Seq(functionExpToTff(l), functionExpToTff(r))))
      case FunctionExpOr(l, r)     => Parenthesized(Or(Seq(functionExpToTff(l), functionExpToTff(r))))
      case FunctionExpBiImpl(l, r) => Parenthesized(BiImpl(functionExpToTff(l), functionExpToTff(r)))
      case FunctionExpApp(n, args) => Appl(UntypedFunSymbol(n), args map functionExpMetaToTff)
      case FunctionExpTrue         => True
      case FunctionExpFalse        => False
      case _                       => throw TransformationError("Encountered unsupported function expression while translating (e.g. if or let expression)")
    }

  /**
   * translate function expressions including MetaVars to terms
   */
  private def functionExpMetaToTff(f: FunctionExpMeta): Term =
    // the only two constructs which can be turned into a term are
    // FunctionMeta and FunctionExpApp (Appl is both a Term and a FofUnitary!)
    // therefore, encountering any other FunctionExpMeta must result in an error!
    f match {
      case FunctionMeta(MetaVar(m)) => UntypedVariable(m)
      case FunctionExpApp(n, args)  => Appl(UntypedFunSymbol(n), args map functionExpMetaToTff)
      case _                        => throw TransformationError("Encountered unexpected construct in functionExpMetaToTff.")
    }

  /**
   * finds constructs in given sequence of TypingRuleJudgment that have the
   * given MetaVar m as direct child and are one of
   * - function applications (Appl)
   * - equalities/inequalities with the m on one side and a function application on the other side
   *
   */
  private def findTypableOccurrences(m: MetaVar)(jdglist: Seq[TypingRuleJudgment]): Set[FunctionExp] = {
    def containsMetaVar(args: Seq[FunctionExpMeta]): Boolean = args exists
      {
        case FunctionMeta(m0) => m == m0
        case _                => false
      }

    def searchFunctionExp(e: FunctionExpMeta): Set[FunctionExp] = {
      e match {
        // base cases just return empty set, all listed explicitly just in case!  
        case FunctionMeta(m)   => Set()
        case FunctionExpVar(n) => Set()
        case FunctionExpTrue   => Set()
        case FunctionExpFalse  => Set()
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
        case FunctionExpNeq(f1, f2)  => searchFunctionExp(f1) ++ searchFunctionExp(f2)
        case FunctionExpAnd(l, r)    => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpOr(l, r)     => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpBiImpl(l, r) => searchFunctionExp(l) ++ searchFunctionExp(r)
        case FunctionExpIf(c, t, e)  => searchFunctionExp(c) ++ searchFunctionExp(t) ++ searchFunctionExp(e)
        case FunctionExpLet(n, e, i) => searchFunctionExp(e) ++ searchFunctionExp(i)
        case fe @ FunctionExpApp(fn, args) => {
          val afe = (args flatMap searchFunctionExp).toSet
          if (containsMetaVar(args)) afe ++ Set(fe) else afe
        }
        // this should never happen
        case _ => throw TransformationError(s"While trying to type meta variable ${m.name}, found a function expression that is not covered by the code!")
      }
    }
    (for (jdg <- jdglist) yield {
      jdg match {
        case FunctionExpJudgment(f)   => searchFunctionExp(f)
        case ExistsJudgment(vl, jdgl) => findTypableOccurrences(m)(jdgl)
        case ForallJudgment(vl, jdgl) => findTypableOccurrences(m)(jdgl)
        case NotJudgment(jdg)         => findTypableOccurrences(m)(Seq(jdg))
        case OrJudgment(jdgll)        => jdgll flatMap findTypableOccurrences(m)
        case _                        => throw TransformationError(s"While trying to type meta variable ${m.name}, encountered a construct in an axiom, premise, or conclusion that is not supported by core modules!")
      }
    }).flatten.toSet
  }

  /**
   * given a FunctionExp such as the ones found by findTypableOccurrences,
   * tries to determine the type of the given MetaVar m
   *
   * TODO: Currently, this function is "nice": if it cannot type the given FunctionExp, it just returns none, no error is thrown.
   * Thinks about whether this function should rather throw errors if it cannot type sth.
   */
  private def typeOcc(m: MetaVar, occ: FunctionExp): Option[TffAtomicType] = {

    def getMetaVarPos(args: Seq[FunctionExpMeta]): Seq[Int] =
      for (i <- args.indices if (args(i) == FunctionMeta(m))) yield i

    def retrieveType(n: String): TffTopLevelType = {
      val types = typedSymbols filter { case TypedSymbol(v, t) => v == n }
      if (types.size == 1) types.head.tfftype
      else throw TransformationError(s"Type for constructor or function ${n} not found or found more than once!")
    }

    def getReturnType(t: TffTopLevelType): Option[TffAtomicType] =
      t match {
        case TffMappingType(_, restype) => Some(restype)
        case st @ SymbolType(t)         => Some(st)
        case _                          => None
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
      case FunctionExpEq(FunctionMeta(mx @ _), FunctionExpApp(n, _)) if (mx == m) => getReturnType(retrieveType(n))
      case FunctionExpNeq(FunctionMeta(mx @ _), FunctionExpApp(n, _)) if (mx == m) => getReturnType(retrieveType(n))
      case FunctionExpApp(fn, args) => getArgType(retrieveType(fn), args)
      case _ => throw TransformationError(s"While trying to type meta variable ${m.name}, an untypable FunctionExp was marked as typable.")
    }
  }

  /**
   * create a list of MetaVars for a quantifier
   * tries to infer the type of each MetaVar to create a TypedVariable
   * if that is not possible (e.g. due to ambiguity) leaves variables untyped
   */
  private def makeVarlist(vars: Seq[MetaVar], jdglist: Seq[TypingRuleJudgment]): Seq[Variable] = {
    val typesPerFV = for (fv <- vars.toList) yield {
      val typelist = for {
        occ <- findTypableOccurrences(fv)(jdglist)
        val t = typeOcc(fv, occ)
        if (t != None)
      } yield t.get
      fv -> typelist.toSet //remove duplicate occurrences of found types; ideally, only one occurrence should be left!
    }

    for ((m, tl) <- typesPerFV) yield {
      tl.size match {
        case 0 => throw TransformationError(s"Trying to type meta variable ${m.name} yielded no possible type!") //has to be an error case, untyped variables will not be supported by e.g. Vampire!
        case 1 => TypedVariable(m.name, tl.head)
        case _ => throw TransformationError(s"Trying to type meta variable ${m.name} yielded several possible types!")
      }
    }
  }

  /**
   * assembles the final TffFile from the individual collections defined above
   * makes sure that the order of the declarations in the generated file is: type declarations, axioms, conjecture
   * TODO find out whether this order is always a good idea!!
   */
  private def constructFinalTff(name: String): TffFile = {
    goal match {
      case Some(g) => TffFile(name, typedecllist ++ axiomlist ++ Seq(g))
      case None    => throw TransformationError(s"There was no goal in Module ${name}; TFF Transformation failed!")
    }
  }

}
