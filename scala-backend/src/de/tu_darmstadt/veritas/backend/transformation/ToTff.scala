package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.fof._
import de.tu_darmstadt.veritas.backend.tff._
import de.tu_darmstadt.veritas.backend.veritas._

/**
 * Transforms Core TSSL (Veritas) Modules to TFF syntax
 *
 * Structure of Core Modules
 * - no imports
 * - section with "symbol declarations" (constructor decls, const decls, function sigs...) (can be empty)
 * - section with n axioms, where typing judgments were already transformed to some typed function! (can be empty)
 * - exactly one goal!
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
   * collects user-defined sorts
   */
  private var sorts: Set[String] = Set()

  /**
   * collects constructor/const and function names and maps them to their Tff type
   */
  private var declmap: Map[String, TffTopLevelType] = Map()

  /**
   * top-level function for translating a Module to a TffFile
   */
  def toTffFile(veritasModule: Module): TffFile = {
    veritasModule match {
      case Module(name, Seq(), body) => {
        try {
          bodyToTff(body)
          constructFinalTff(name)
        } catch {
          case TransformationError(s) => throw TransformationError(s"Failed to transform Module ${name} to TFF:" + s)
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
    body foreach { md =>
      md match {
        case Axioms(axs) => axiomlist ++= translateAxioms(axs)
        case Goals(gs, _) => if (goal != None | gs.length > 1)
          throw TransformationError("More than one goal found!")
        else goal = Some(translateGoal(gs(0)))
        case Constructors(cs) => addConstDecl(cs)
        case Consts(cs)       => addConstDecl(cs)
        case Sorts(s)         => addSortDef(s)
        case Functions(fds)   => addConstDecl(getFunctionSigs(fds))
        case _                => throw TransformationError("Unsupported top-level construct!")

      }
    }
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
        if (sorts contains name)
          throw TransformationError(s"Sort ${name} has been defined twice!")
        else
          sorts += name

        TffAnnotated(s"${name}_type", Type, TypedSymbol(name, DefinedType("tType")))
      })

  /**
   * collects newly discovered constructor/const/function declarations in declmap variable (mapping their names to the appropriate Tff type),
   * adds a top-level type declaration to typedecllist
   *
   * throws an error if the referenced sorts are not present in sorts set
   */
  private def addConstDecl(cs: Seq[ConstructorDecl]): Unit = ???

  /**
   * extracts function signatures from function definitions, if definitions are empty
   * transforms function signatures to constructor declarations
   *
   * throws an error if the function definitions are not empty (not supported by core modules!)
   */
  private def getFunctionSigs(fds: Seq[FunctionDef]): Seq[ConstructorDecl] = ???

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
   */
  private def typingRuleToTff(prems: Seq[TypingRuleJudgment], conseqs: Seq[TypingRuleJudgment]) =
    Impl(Parenthesized(And(prems map jdgtoTff)), Parenthesized(And(conseqs map jdgtoTff)))

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
      case FunctionExpNot(f)            => Not(functionExpToTff(f))
      case FunctionExpEq(f1, f2)        => Eq(functionExpMetaToTff(f1), functionExpMetaToTff(f2))
      case FunctionExpNeq(f1, f2)       => NeqEq(functionExpMetaToTff(f1), functionExpMetaToTff(f2))
      case FunctionExpAnd(l, r)         => Parenthesized(And(Seq(functionExpToTff(l), functionExpToTff(r))))
      case FunctionExpOr(l, r)          => Parenthesized(Or(Seq(functionExpToTff(l), functionExpToTff(r))))
      case FunctionExpBiImpl(l, r)      => Parenthesized(BiImpl(functionExpToTff(l), functionExpToTff(r)))
      case FunctionExpApp(n, args @ _*) => Appl(UntypedFunSymbol(n), (args map functionExpMetaToTff): _*)
      case FunctionExpTrue              => True
      case FunctionExpFalse             => False
      case _                            => throw TransformationError("Encountered unsupported function expression while translating (e.g. if or let expression)")
    }

  /**
   * translate function expressions including MetaVars to terms
   */
  private def functionExpMetaToTff(f: FunctionExpMeta): Term =
    // the only two constructs which can be turned into a term are
    // FunctionMeta and FunctionExpApp (Appl is both a Term and a FofUnitary!)
    // therefore, encountering any other FunctionExpMeta must result in an error!
    f match {
      case FunctionMeta(MetaVar(m))     => UntypedVariable(m)
      case FunctionExpApp(n, args @ _*) => Appl(UntypedFunSymbol(n), (args map functionExpMetaToTff): _*)
      case _                            => throw TransformationError("Encountered unexpected construct in functionExpMetaToTff.")
    }

  /**
   * create a list of MetaVars for a quantifier
   * tries to infer the type of each MetaVar to create a TypedVariable
   * if that is not possible (e.g. due to ambiguity) leaves variables untyped
   */
  private def makeVarlist(vars: Seq[MetaVar], jdglist: Seq[TypingRuleJudgment]): Seq[Variable] = ???

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