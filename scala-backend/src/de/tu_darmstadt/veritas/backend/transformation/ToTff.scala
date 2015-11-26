package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.fof._
import de.tu_darmstadt.veritas.backend.tff._
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.util.FreeVariables
import de.tu_darmstadt.veritas.backend.Configuration

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

  
  def typedSymbols = CollectTypes.typedSymbols

  /**
   * top-level function for translating a Module to a TffFile
   */
  def toTffFile(veritasModule: Module)(implicit config: Configuration): TffFile = {
    //make sure every mutable state is initialized when applying this!
    axiomlist = Seq()
    goal = None
    
    CollectTypes.apply(Seq(veritasModule))
    
    veritasModule match {
      case Module(name, Seq(), body) => {
        bodyToTff(body)
        constructFinalTff(name + ".tff")
      }
      case Module(name, _, _) => throw TransformationError(s"Failed to transform Module ${name} to TFF: Module still contained imports!")
    }
  }

  /**
   *  translate a Module body (sequence of ModuleDefs) to TffAnnotated
   *  adds the collected declarations to the appropriate collections of the object defined above
   */
  private def bodyToTff(body: Seq[ModuleDef]): Unit = {
    body foreach {
      case d: DataType => addDataType(d)
      case _ =>
    }
    
    body.dropRight(1) foreach { md =>
      md match {
        case Axioms(axs)           => axiomlist ++= translateAxioms(axs)
        case Goals(gs, _)          => throw TransformationError("Found goal in Module which was not at last position!")
        case DataType(_, name, cs) => addDataTypeConstructor(cs, name)
        case Consts(cs, _)         => addConstDecl(cs)
        case Sorts(s)              => addSortDef(s)
        case Functions(fds)        => addFunctionDefinition(fds)
        case PartialFunctions(fds) => addFunctionDefinition(fds)
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
  private def getTopLevelSymbol(name: String): TypedSymbol = typedSymbols.get(name) match {
    case Some(ts) => ts
    case None => throw TransformationError(s"Could find type of symbol $name")
  }

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
        val ts = getTopLevelSymbol(name)
        TffAnnotated(s"${name}_type", Type, ts)
      })

  /**
   * collects newly discovered data types,
   * adds top-level type declaration to typedecllist
   *
   * throws an error if the data type is already defined
   */
  private def addDataType(d: DataType): Unit =
    typedecllist :+= {
      // note: data types can never contain predefined sorts (ensured via "require")
      // so blindly adding the DataType is ok
      val ts = getTopLevelSymbol(d.name)
      TffAnnotated(s"${d.name}_type", Type, ts)
    }

  private def translatePredefinedType(t: String): DefinedType =
    t match {
      case "Bool"  => DefinedType("o")
      case "iType" => DefinedType("i")
      case n       => DefinedType(n)
    }

  /**
   * adds a top-level type declaration to typedecllist
   *
   * throws an error if the referenced sorts are not present in sorts set
   */
  private def addConstDecl(cs: Seq[ConstDecl]): Unit =
    typedecllist ++=
      (for (ConstDecl(name, out) <- cs) yield {
        val outt = CollectTypes.makeAtomicType(out.name)
        val ts = TypedSymbol(name, outt)
        TffAnnotated(s"${name}_type", Type, ts)
      })

  private def addDataTypeConstructor(cs: Seq[DataTypeConstructor], dataType: String): Unit =
    typedecllist ++=
      (for (DataTypeConstructor(name, in) <- cs) yield {
        val outt = CollectTypes.makeAtomicType(dataType)
        val ints = in map (s => CollectTypes.makeAtomicType(s.name))
        val t = if (ints.isEmpty) outt else TffMappingType(ints, outt)
        val ts = TypedSymbol(name, t)
        TffAnnotated(s"${name}_type", Type, ts)
      })

  /**
   * throws an error if the function equations are not empty (not supported by core modules!)
   */
  private def addFunctionDefinition(cs: Seq[FunctionDef]): Unit =
    typedecllist ++=
      (for (FunctionDef(FunctionSig(name, in, out), eqs) <- cs) yield {
        if (!eqs.isEmpty)
          throw TransformationError(s"Function definition ${name} still contained untransformed function equations!")
        
        val outt = CollectTypes.makeAtomicType(out.name)
        val ints = in map (s => CollectTypes.makeAtomicType(s.name))
        val t = if (ints.isEmpty) outt else TffMappingType(ints, outt)
        val ts = TypedSymbol(name, t)
        TffAnnotated(s"${name}_type", Type, ts)
      })

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

  def makeVarlist(vars: Seq[MetaVar], jdgs: Seq[TypingRuleJudgment]): Seq[Variable] = {
    val map = CollectTypes.inferMetavarTypes(vars, jdgs)
    for (v <- vars)
      yield TypedVariable(v.name, map(v))
  }
  
  /**
   * translates typing rules (= implications) to Tff
   * TODO: universal quantification over all free variables!!
   */
  private def typingRuleToTff(prems: Seq[TypingRuleJudgment], conseqs: Seq[TypingRuleJudgment]) = {
    val quantifiedVars = FreeVariables.freeVariables(prems ++ conseqs)
		val vars = makeVarlist(quantifiedVars.toSeq, prems ++ conseqs)

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
