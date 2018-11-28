package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.util.BackendError

import scala.collection.mutable

object LemmaEquivalence {
  val Bottom = MetaVar("x")

  /**
    * A module transformation that takes a mapping from MetaVar to MetaVar
    * and can be applied to a `TypingRuleJudgment` object to rename all
    * variables in the judgment accordingly.
    * @param renaming function mapping metavariables to metavaraibles
    */
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

  /**
    * Companion object that applies a renaming to a typing rule judgment and returns the result.
    */
  private object VariableRenamer {
    def apply(vc: TypingRuleJudgment, renaming: MetaVar => MetaVar): TypingRuleJudgment = {
      new VariableRenamer(renaming)(vc)
    }
  }

  /**
    * A module transformation that does not actually transform a module. Rather,
    * it takes two VeritasConstruct objects `ref` and `rule` that should only
    * differ in their variable namings, and proposes a bijective renaming of
    * variables in `rule` such that the resulting object is equal to `ref`.
    * In case no such renaming can be found, a `RenamingError` is thrown.
    */
  private class FindRenaming {
    val renaming: mutable.HashMap[MetaVar, MetaVar] = new mutable.HashMap()

    def visit(ref: VeritasConstruct, rule: VeritasConstruct): Unit = {
      // Check that `ref` is a MetaVar instance iff. `rule` is a MetaVar instance. Otherwise,
      // the ASTs of `ref` and `rule` do not match.
      if(ref.isInstanceOf[MetaVar] != rule.isInstanceOf[MetaVar])
        throw RenamingError(s"Got one MetaVar, one non-MetaVar: $ref, $rule")
      ref match {
        case MetaVar(_) => tryRename(rule.asInstanceOf[MetaVar], ref.asInstanceOf[MetaVar])
        // For Forall and Exists judgments, ensure that they do not bind variables that
        // we have already decided to rename -- this would mean variable shadowing, which
        // we do not support.
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

    /** Propose a renaming of all occurrences of `from` in `rule` to `to`.
      * This may fail if `from` is already renamed to a variable different
      * from `to`, or if the resulting renaming is not a bijective renaming.
      */
    def tryRename(from: MetaVar, to: MetaVar): Unit = {
      if(renaming.contains(from) && renaming(from) != to)
        throw RenamingError(s"$from already renamed to ${renaming(from)} instead of $to")
      renaming += (from -> to)
      if(renaming.size != renaming.values.toSet.size)
        throw RenamingError("can only use bijective renamings")
    }

    /** Recursively visit all children of `ref` and `rule` while checking
      * that they have compatible shapes. */
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

  /** Companion object that tries to find a renaming for `ref` and `rule` and
    * returns it. Throws a `RenamingError` if no such renaming can be found. */
  private object FindRenaming {
    def apply(ref: VeritasConstruct, rule: VeritasConstruct): Map[MetaVar, MetaVar] = {
      val find = new FindRenaming
      find.visit(ref, rule)
      find.renaming.toMap
    }
  }

  case class RenamingError(msg: String) extends BackendError("Renaming failed. " + msg)
  case class ReorderingError(msg: String) extends BackendError("Reordering failed. " + msg)

  /** Replace all variables in a sequence of judgments with ``Bottom`` and return the resulting sequence */
  def replaceVarsWithBottom(judgments: Seq[TypingRuleJudgment]): Seq[TypingRuleJudgment] = {
    judgments.map(VariableRenamer(_, x => Bottom))
  }

  /** Reorder the typing judgments in `rule`, the result being `rule2`, such that `rule` and `rule2`
    * differ only in the variable namings. If such a reordering cannot be found, raise a `ReorderingError`.
    */
  def reorderTypingJudments(ref: Seq[TypingRuleJudgment], rule: Seq[TypingRuleJudgment]): Seq[TypingRuleJudgment] = {
    // replace all variables with Bottom
    val ref_bottom = replaceVarsWithBottom(ref)
    val rule_bottom = replaceVarsWithBottom(rule)
    // If `ref` and `rule` have the same length and the judgments, if considered as sets,
    // are equal if their variables are renamed to Bottom, we might be able to reorder
    // the judgments!
    if(ref_bottom.toSet == rule_bottom.toSet
      && ref.length == rule.length) {
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

  /**
    * Try to reorder the premises and consequences of `rule` such that they only differ from
    * `ref` by their variable namings. If that is not possible, return None.
    */
  def reorderTypingRule(ref: TypingRule, rule: TypingRule): Option[TypingRule] = {
    try {
      val newPremises = reorderTypingJudments(ref.premises, rule.premises)
      val newConsequences = reorderTypingJudments(ref.consequences, rule.consequences)
      Some(TypingRule(rule.name, newPremises, newConsequences))
    } catch {
      case _: ReorderingError => None
    }
  }

  /**
    * Given two rules `ref` and `rule` that differ only in their variable namings,
    * try to find a renaming such that they are equal. Return None if impossible.
    */
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

  /**
    * return true if `ref` and `rule` are "~"-equivalent: This is the case if the variables of `rule`
    * can be renamed (using a bijective renaming), the result being `rule2`,
    * and the premises and consequences of `rule2` can be reordered, the result being `rule3`,
    * such that `ref` == `rule3` (on the AST leve), disregarding the name.
    * @param ref reference lemma
    * @param rule compared lemma
    * @return true or false
    */
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
