package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExp, FunctionExpMeta}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.util.BackendError

import scala.collection.mutable

object LemmaEquivalence {
  val bottom = MetaVar("x")

  private class VariableRenamer(renaming: MetaVar => MetaVar) extends ModuleTransformation with Serializable {
     def apply(vc: TypingRuleJudgment): TypingRuleJudgment = {
      transTypingRuleJudgments(vc) match {
        case Seq(result) => result
        case _ => throw BackendError(s"Multiple results!")
      }
    }

    override def transMetaVars(m: MetaVar): Seq[MetaVar] = {
      Seq(renaming(m))
    }

    override def transMetaVar(m: MetaVar): MetaVar = {
      renaming(m)
    }
  }

  private object VariableRenamer {
    def apply(vc: TypingRuleJudgment, renaming: MetaVar => MetaVar): TypingRuleJudgment = {
      new VariableRenamer(renaming)(vc)
    }
  }

  private class FindRenaming {
    var renaming: mutable.HashMap[MetaVar, MetaVar] = new mutable.HashMap()

    def visit(ref: VeritasConstruct, rule: VeritasConstruct): Unit = {
      // check that the shapes are equal
      if(ref.isInstanceOf[MetaVar] != rule.isInstanceOf[MetaVar])
        throw RenamingError(s"Got one MetaVar, one non-MetaVar: $ref, $rule")
      ref match {
        case MetaVar(_) => tryRename(rule.asInstanceOf[MetaVar], ref.asInstanceOf[MetaVar])
        case ExistsJudgment(varlist, _) =>
          if(varlist.exists(renaming contains _))
            throw RenamingError("Variable shadowing in ExistsJudgment is unsupported")
        case ForallJudgment(varlist, _) =>
          if(varlist.exists(renaming contains _))
            throw RenamingError("Variable shadowing in ForallJudgment is unsupported")
        case _ =>
      }
      visitChildren(ref, rule)
    }

    def tryRename(from: MetaVar, to: MetaVar): Unit = {
      if(renaming.contains(from) && renaming(from) != to)
        throw RenamingError(s"$from already renamed to ${renaming(from)} instead of $to")
      renaming += (from -> to)
      if(renaming.size != renaming.values.size)
        throw RenamingError("can only use bijective renamings")
    }

    def visitChildren(ref: VeritasConstruct, rule: VeritasConstruct): Unit = {
      if(ref.children.length != rule.children.length)
        throw RenamingError(s"Incompatible shapes: $ref, $rule")
      (ref.children zip rule.children).foreach {
        case (refGroup, ruleGroup) => {
          if(refGroup.length != ruleGroup.length)
            throw RenamingError(s"Incompatible shapes: $refGroup, $ruleGroup")
          (refGroup zip ruleGroup).foreach {
            case (refChild, ruleChild) => visit(refChild, ruleChild)
          }
        }
      }
    }
  }

  private object FindRenaming {
    def apply(ref: VeritasConstruct, rule: VeritasConstruct): Map[MetaVar, MetaVar] = {
      val find = new FindRenaming
      find.visit(ref, rule)
      find.renaming.toMap
    }
  }

  case class RenamingError(msg: String) extends BackendError("Renaming failed. " + msg)
  case class ReorderingError(msg: String) extends BackendError("Reordering failed. " + msg)

  def replaceVarsWithBottom(judgments: Seq[TypingRuleJudgment]): Seq[TypingRuleJudgment] = {
    judgments.map(VariableRenamer(_, x => bottom))
  }

  def reorderTypingJudments(ref: Seq[TypingRuleJudgment], rule: Seq[TypingRuleJudgment]): Seq[TypingRuleJudgment] = {
    val ref_bottom = replaceVarsWithBottom(ref)
    val rule_bottom = replaceVarsWithBottom(rule)
    if(ref_bottom.toSet == rule_bottom.toSet
      && ref_bottom.length == rule_bottom.length) {
      val remainingRule_bottom: mutable.ListBuffer[Option[TypingRuleJudgment]] = mutable.ListBuffer(rule_bottom.map(Some(_)):_*)
      ref_bottom.map(judgment_bottom => {
        val idx = remainingRule_bottom.indexOf(Some(judgment_bottom))
        if(idx == -1)
          throw ReorderingError("Same bottom premises, but incompatible shapes")
        remainingRule_bottom.update(idx, None)
        rule(idx)
      })
    } else {
      throw ReorderingError("Incompatible shapes")
    }
  }

  def reorderTypingRule(ref: TypingRule, rule: TypingRule): Option[TypingRule] = {
    try {
      val newPremises = reorderTypingJudments(ref.premises, rule.premises)
      val newConsequences = reorderTypingJudments(ref.consequences, rule.consequences)
      Some(TypingRule(rule.name, newPremises, newConsequences))
    } catch {
      case _: ReorderingError => None
    }
  }

  def renameVariables(ref: TypingRule, rule: TypingRule): Option[TypingRule] = {
    try {
      val renaming = FindRenaming(ref, rule)
      Some(TypingRule(rule.name,
        rule.premises.map(VariableRenamer(_, renaming)),
        rule.consequences.map(VariableRenamer(_, renaming))
      ))
    } catch {
      case _: RenamingError => None
    }
  }

  def isEquivalent(ref: TypingRule, rule: TypingRule): Boolean = {
    reorderTypingRule(ref, rule) match {
      case Some(reordered) => {
        renameVariables(ref, reordered) match {
          case Some(renamed) => renamed.premises == ref.premises && renamed.consequences == ref.consequences
          case None => false
        }
      }
      case None => false
    }
  }
}
