package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.fof._
import de.tu_darmstadt.veritas.backend.tff._
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.util.FreeVariables
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectTypesClass
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectTypes
import de.tu_darmstadt.veritas.backend.transformation.collect.TypeInference

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
   * collects user-defined atomic types
   * (sorts and simple types, already translated to typed symbols)
   */
  private var typedSymbols: Map[String, TypedSymbol] = Map()

  /**
   * list for collecting the axioms in the module
   */
  private var axiomlist: Seq[TffAnnotated] = Seq()

  /**
   * variable for collecting the goal - if empty at the end, then throw exception!
   */
  private var goal: Option[TffAnnotated] = None

  private var types: CollectTypes = _

  /**
   * top-level function for translating a Module to a TffFile
   */
  def toTffFile(veritasModule: Module)(implicit config: Configuration): TffFile = {
    //make sure every mutable state is initialized when applying this!
    typedecllist = Seq()
    axiomlist = Seq()
    goal = None

    types = new CollectTypesClass
    types.apply(Seq(veritasModule))

    typedSymbols = Map()
    for (d <- types.dataTypes)
      addDataType(d)
    for ((con, (in, out)) <- types.constrTypes)
      addSymbol(con, in, out)
    for ((con, (in, out)) <- types.functypes)
      addSymbol(con, in, out)
    for ((con, (in, out)) <- types.pfunctypes)
      addSymbol(con, in, out)

    veritasModule match {
      case Module(name, Seq(), body) => {
        bodyToTff(body)
        constructFinalTff(name + ".tff")
      }
      case Module(name, _, _) => throw TransformationError(s"Failed to transform Module ${name} to TFF: Module still contained imports!")
    }
  }

  /**
   * create a top-level typed symbol
   */
  private def makeTopLevelSymbol(name: String): TypedSymbol =
    TypedSymbol(name, DefinedType("tType"))

  private def addTSIfNew(ts: TypedSymbol): Unit =
    if (typedSymbols.contains(ts.name))
      throw TransformationError(s"Sort, Constructor, or function ${ts.name} has been defined twice!")
    else
      typedSymbols += ts.name -> ts

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
  def getAtomicType(name: String): TffAtomicType =
    if (SortDef.predefinedSorts contains name)
      translatePredefinedType(name)
    else {
      val ts = makeTopLevelSymbol(name)
      typedSymbols.get(name) match {
        case Some(ts) => SymbolType(ts)
        case None     => throw TransformationError(s"Encountered sort reference ${name}, which has not been defined yet!")
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
        case DataType(_, name, cs) => {}
        case Consts(cs, _)         => {}
        case Sorts(s)              => {}
        case Functions(fds)        => {}
        case PartialFunctions(fds) => {}
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
   * collects newly discovered data types,
   * adds top-level type declaration to typedecllist
   *
   * throws an error if the data type is already defined
   */
  private def addDataType(d: String): Unit =
    typedecllist :+= {
      // note: data types can never contain predefined sorts (ensured via "require")
      // so blindly adding the DataType is ok
      val ts = makeTopLevelSymbol(d)
      addTSIfNew(ts)
      TffAnnotated(s"${d}_type", Type, ts)
    }

  private def addSymbol(name: String, in: Seq[SortRef], out: SortRef): Unit =
    typedecllist :+= {
      val outt = getAtomicType(out.name)
      val ints = in map (s => getAtomicType(s.name))
      val t = if (ints.isEmpty) outt else TffMappingType(ints, outt)
      val ts = TypedSymbol(name, t)
      addTSIfNew(ts)
      TffAnnotated(s"${name}_type", Type, ts)
    }

  /**
   * translates axioms to Tff
   */
  private def translateAxioms(axs: Seq[TypingRule]): Seq[TffAnnotated] =
    axs map {
      case TypingRule(name, prems, conseqs) => TffAnnotated(name, Axiom, typingRuleToTff(prems, conseqs))
    }

  /**
   * translates goals to Tff
   */
  private def translateGoal(g: TypingRule): TffAnnotated =
    g match {
      case TypingRule(name, prems, conseqs) =>
        TffAnnotated(name, Conjecture, typingRuleToTff(prems, conseqs))
    }

  def makeVarlist(vars: Seq[MetaVar], jdgs: Seq[TypingRuleJudgment]): Seq[Variable] = {
    for (v <- vars)
      yield TypedVariable(v.name, getAtomicType(v.sortType.name))
  }

  /**
   * translates typing rules (= implications) to Tff
   * TODO: universal quantification over all free variables!!
   */
  private def typingRuleToTff(prems: Seq[TypingRuleJudgment], conseqs: Seq[TypingRuleJudgment]) = {
    val quantifiedVars = FreeVariables.freeVariables(prems ++ conseqs)
    val jdgs = prems ++ conseqs
    types.inferMetavarTypes(quantifiedVars, jdgs)
    val vars = makeVarlist(quantifiedVars.toSeq, jdgs)

    val transformedprems = prems map jdgtoTff

    if (transformedprems == Seq(True))
      ForAll(vars, Parenthesized(And(conseqs map jdgtoTff)))
    else
      ForAll(vars, Parenthesized(
        Impl(Parenthesized(And(transformedprems)), Parenthesized(And(conseqs map jdgtoTff)))))
  }

  /**
   * translates individual clauses (premises or conclusion) to Tff (-> FofUnitary)
   */
  private def jdgtoTff(jdg: TypingRuleJudgment): FofUnitary =
    jdg match {
      case FunctionExpJudgment(f) => functionExpToTff(f)
      case ExistsJudgment(vars, jdglist) => {
        val mappedvars = makeVarlist(vars, jdglist)
        if (mappedvars.isEmpty)
          Parenthesized(And(jdglist map jdgtoTff))
        else
          Exists(mappedvars, Parenthesized(And(jdglist map jdgtoTff)))
      }
      case ForallJudgment(vars, jdglist) => {
        val mappedvars = makeVarlist(vars, jdglist)
        if (mappedvars.isEmpty)
          Parenthesized(And(jdglist map jdgtoTff))
        else
          ForAll(mappedvars, Parenthesized(And(jdglist map jdgtoTff)))
      }
      case NotJudgment(jdg) => Not(jdgtoTff(jdg))
      case OrJudgment(ors) => {
        val translatedors = ors map (orcase => Parenthesized(And(orcase map jdgtoTff)))
        if (translatedors.isEmpty)
          True
        else if (translatedors.length == 1)
          translatedors.head
        else Parenthesized(Or(translatedors))
      }
      case _ => throw TransformationError("Encountered unsupported judgment while translating a goal or axiom (e.g. typing judgment)")
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
   * assembles the final TffFile from the individual collections defined above
   * makes sure that the order of the declarations in the generated file is: type declarations, axioms, conjecture
   * TODO find out whether this order is always a good idea!!
   */
  private def constructFinalTff(name: String): TffFile = {
    goal match {
      case Some(g) => TffFile(name, g.name, typedecllist ++ axiomlist ++ Seq(g))
      case None    => throw TransformationError(s"There was no goal in Module ${name}; TFF Transformation failed!")
    }
  }
}
