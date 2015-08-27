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
 * - section with n axioms (can be empty)
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

  def toTffFile(veritasModule: Module): TffFile = {
    veritasModule match {
      case Module(name, Seq(), body) => {
        try {
          bodyToTff(body)
          constructFinalTff(name)
        } catch {
          case TransformationError(s) => throw TransformationError(s"Module ${name} was to be transformed to TFF, but contained elements not supported by core modules:" + s)
        }
      }
      case Module(name, _, _) => throw TransformationError(s"Module ${name} was to be transformed to TFF, but still contained imports!")
    }
  }

  private def bodyToTff(body: Seq[ModuleDef]): Unit = {
    body foreach { md =>
      md match {
        case Axioms(axs) => axiomlist ++ translateAxioms(axs)
        case Goals(gs, _) => if (goal != None | gs.length > 1)
          throw TransformationError("More than one goal found!")
        else goal = Some(translateGoal(gs(0)))
        case Constructors(cs) => ???
        case Consts(cs)       => ???
        case Sorts(s)         => ???
        case Functions(fds)   => ???
        case _                => throw TransformationError("Unsupported top-level construct!")

      }
    }
  }

  private def translateAxioms(axs: Seq[TypingRule]): Seq[TffAnnotated] =
    for (a <- axs)
      yield a match {
      case TypingRule(name, prems, conseqs) =>
        TffAnnotated(name, Axiom, typingRuleToTff(prems, conseqs))
    }
  
  private def translateGoal(g: TypingRule): TffAnnotated = 
    g match {
    case TypingRule(name, prems, conseqs) =>
      TffAnnotated(name, Conjecture, typingRuleToTff(prems, conseqs))
  }
  
  private def typingRuleToTff(prems: Seq[TypingRuleJudgment], conseqs: Seq[TypingRuleJudgment]) =
    Impl(makeAnd(prems map jdgtoTff), makeAnd(conseqs map jdgtoTff))

  
  
  private def jdgtoTff(jdg: TypingRuleJudgment): FofUnitary = ???
  
  private def makeAnd(formulas: Seq[FofUnitary]): FofUnitary = ???
  

  private def constructFinalTff(name: String): TffFile = {
    goal match {
      case Some(g) => TffFile(name, typedecllist ++ axiomlist ++ Seq(g))
      case None    => throw TransformationError(s"There was no goal in Module ${name}; TFF Transformation failed!")
    }
  }

}