package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.fof._
import de.tu_darmstadt.veritas.backend.util.BackendError
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintable
import de.tu_darmstadt.veritas.backend.util.prettyprint.PrettyPrintWriter
import de.tu_darmstadt.veritas.backend.util.FreeVariables
import de.tu_darmstadt.veritas.backend.fof.Variable

/**
 * Transforms Core TSSL (Veritas) Modules to FOF syntax
 *
 * Structure of Core Modules
 * - no imports
 * - section with "symbol declarations" (constructor decls, const decls, function sigs...) (can be empty)
 * - section with n axioms, where typing judgments were already transformed to some typed function! (can be empty)
 * - exactly one goal!
 */
object ToFof {
  def toFofFile(coreModule: Module): FofFile = coreModule match {
    case Module(name, Seq(), body) => {
      val goal = getOnlyGoal(coreModule)
      val axioms = coreModule.body.collect{ case Axioms(axioms) => axioms }.flatten

      val transformedAxioms = axioms map (typingRuleToFof(_, Axiom))
      val transformedGoal = typingRuleToFof(goal, Conjecture)
      
      FofFile(name + ".fof", transformedAxioms :+ transformedGoal)
    }
    case Module(name, _, _) => throw TransformationError(s"(Core TSSL) Module $name still contained imports")
  }

  /**
   * returns the single goal, throws TransformationError if
   *  - there is no goal (every .fof file must have one for proof)
   *  - there is more than one (fof allows only one per file)
   */
  private def getOnlyGoal(mod: Module): TypingRule = {
    val allGoals = mod.body.collect{ case Goals(goals, /* TODO */ timeout) => goals }.flatten 
    allGoals match {
      case Seq() => throw TransformationError(s"(Core TSSL) Module ${mod.name} contained no goal")
      case Seq(singleGoal) => singleGoal
      case _ => throw TransformationError(s"(Core TSSL) Module ${mod.name} contained more than one goal")
    }
  }
  
  private def typingRuleToFof(rule: TypingRule, role: FormulaRole): FofAnnotated =
    FofAnnotated(rule.name, role, typingRuleToFof(rule.premises, rule.consequences))
  
  private def typingRuleToFof(prems: Seq[TypingRuleJudgment], conseqs: Seq[TypingRuleJudgment]): Fof = {
    val quantifiedVars = FreeVariables.freeVariables(prems ++ conseqs) map toUntypedVar
    ForAll(quantifiedVars.toSeq, Parenthesized( 
        Impl(Parenthesized(And(prems map jdgToFof)), Parenthesized(And(conseqs map jdgToFof)))))
  }

  /**
   * translates individual clauses (premises or conclusion)
   */
  private def jdgToFof(jdg: TypingRuleJudgment): FofUnitary =
    jdg match {
      case FunctionExpJudgment(f)        => functionExpToFof(f)
      case ExistsJudgment(vars, jdglist) => Exists(vars map toUntypedVar, Parenthesized(And(jdglist map jdgToFof)))
      case ForallJudgment(vars, jdglist) => ForAll(vars map toUntypedVar, Parenthesized(And(jdglist map jdgToFof)))
      case NotJudgment(jdg)              => Not(jdgToFof(jdg))
      case OrJudgment(ors)               => Parenthesized(Or(ors map (orcase => Parenthesized(And(orcase map jdgToFof)))))
      case _                             => throw TransformationError("Encountered unsupported (not Core) judgment while translating a goal or axiom (e.g. typing judgment): " + jdg)
    }
  
  private def toUntypedVar(v: MetaVar): UntypedVariable = UntypedVariable(v.name)

  /**
   * translate individual function expressions
   * outer function expressions cannot be MetaVars, since a MetaVar cannot be translated to a FofUnitary
   */
  private def functionExpToFof(f: FunctionExp): FofUnitary =
    f match {
      case FunctionExpNot(f)            => Not(functionExpToFof(f))
      case FunctionExpEq(f1, f2)        => Eq(functionExpMetaToFof(f1), functionExpMetaToFof(f2))
      case FunctionExpNeq(f1, f2)       => NeqEq(functionExpMetaToFof(f1), functionExpMetaToFof(f2))
      case FunctionExpAnd(l, r)         => Parenthesized(And(Seq(functionExpToFof(l), functionExpToFof(r))))
      case FunctionExpOr(l, r)          => Parenthesized(Or(Seq(functionExpToFof(l), functionExpToFof(r))))
      case FunctionExpBiImpl(l, r)      => Parenthesized(BiImpl(functionExpToFof(l), functionExpToFof(r)))
      case FunctionExpApp(n, args @ _*) => Appl(UntypedFunSymbol(n), (args map functionExpMetaToFof): _*)
      case FunctionExpTrue              => True
      case FunctionExpFalse             => False
      case _                            => throw TransformationError("Encountered unsupported (not Core) function expression while translating (e.g. if or let expression): " + f)
    }

  /**
   * translate function expressions including MetaVars to terms
   */
  private def functionExpMetaToFof(f: FunctionExpMeta): Term =
    // the only two constructs which can be turned into a term are
    // FunctionMeta and FunctionExpApp (Appl is both a Term and a FofUnitary!)
    // therefore, encountering any other FunctionExpMeta must result in an error!
    f match {
      case FunctionMeta(MetaVar(m))     => UntypedVariable(m)
      case FunctionExpApp(n, args @ _*) => Appl(UntypedFunSymbol(n), (args map functionExpMetaToFof): _*)
      case _                            => throw TransformationError("Encountered unexpected construct in functionExpMetaToTff: " + f)
    }
}
