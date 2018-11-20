package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionExp, FunctionExpMeta}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.util.BackendError

import scala.collection.mutable

object AlphaEquivalence {
  val bottom = MetaVar("x")

  private class JudgmentTraverser extends ModuleTransformation with Serializable {
    def apply(vc: TypingRule): Seq[TypingRule] = {
      transTypingRules(vc)
    }

    def apply(vc: TypingRuleJudgment): Seq[TypingRuleJudgment] = {
      transTypingRuleJudgments(vc)
    }
  }

  private class VariableRenamer(ref: TypingRule, rule: TypingRule) extends ModuleTransformation with Serializable {
    var renaming: mutable.HashMap[MetaVar, MetaVar] = new mutable.HashMap()

    def apply(): Map[MetaVar, MetaVar] = {
      transTypingRules(rule)
      renaming.toMap
    }

    override def transMetaVars(m: MetaVar): Seq[MetaVar] = {
      Seq(m)
    }

    override def transMetaVar(m: MetaVar): MetaVar = {
      val refChoices = walkPathInRef().filter(_.isInstanceOf[MetaVar]).toSet
      if(refChoices.size > 1) {
        throw RenamingError("more than one choice for reference rule traversal") // TODO
      } else if(refChoices.isEmpty) {
        throw RenamingError("could not traverse reference rule")
      } else {
        val choice = refChoices.head
        if(!choice.isInstanceOf[MetaVar])
          throw RenamingError(s"$choice is not a MetaVariable")
        tryRename(m, choice.asInstanceOf[MetaVar])
        m
      }
    }

    def tryRename(from: MetaVar, to: MetaVar): Unit = {
      if(renaming.contains(from) && renaming(from) != to)
        throw RenamingError(s"$from already renamed to ${renaming(from)} instead of $to")
      renaming += (from -> to)
    }

    def walkPathInRef(): Seq[VeritasConstruct] = {
      walkInRef(path.reverse, ref, rule)
    }

    def walkInRef(path: Seq[VeritasConstruct],
                  refPosition: VeritasConstruct,
                  rulePosition: VeritasConstruct): Seq[VeritasConstruct] = path match {
      case Nil => Seq(refPosition)
      case hd :: tl => {
        // get the position of ``hd`` in rule positions's children
        val indices = rulePosition.children.zipWithIndex.flatMap {
          case (ch, idx1) => ch.zipWithIndex.collect {
            case (child, idx2) if child == hd => (idx1, idx2)
          }
        }
        indices.flatMap {
          case (idx1, idx2) => {
            val newRefPosition = refPosition.children(idx1)(idx2)
            walkInRef(tl, newRefPosition, hd)
          }
        }
      }
    }
  }

  case class RenamingError(msg: String) extends BackendError("Renaming failed. " + msg)


  def replaceMetaVars[T](judgment: TypingRuleJudgment, renaming: MetaVar => MetaVar): VeritasConstruct = {
    val replacer = new JudgmentTraverser {
      override def transMetaVars(m: MetaVar): Seq[MetaVar] = {
        Seq(renaming(m))
      }

      override def transMetaVar(m: MetaVar): MetaVar = {
        renaming(m)
      }
    }
    replacer(judgment).head
  }

  def replaceVarsWithBottom(judgments: Seq[TypingRuleJudgment]): Seq[TypingRuleJudgment] = {
    judgments.map(replaceMetaVars(_, x => bottom)).asInstanceOf[Seq[TypingRuleJudgment]]
  }

  def reorderTypingJudments(ref: Seq[TypingRuleJudgment], rule: Seq[TypingRuleJudgment]): Option[Seq[TypingRuleJudgment]] = {
    val ref_bottom = replaceVarsWithBottom(ref)
    val rule_bottom = replaceVarsWithBottom(rule)
    if(ref_bottom.toSet == rule_bottom.toSet
      && ref_bottom.length == rule_bottom.length) {
      Some(ref_bottom.map(rule_bottom.indexOf(_)).map(rule))
    } else {
      None
    }
  }

  def reorderTypingRule(ref: TypingRule, rule: TypingRule): Option[TypingRule] = {
    reorderTypingJudments(ref.premises, rule.premises) match {
      case Some(newPremises) => reorderTypingJudments(ref.consequences, rule.consequences) match {
        case Some(newConsequences) => Some(TypingRule(rule.name, newPremises, newConsequences))
        case None => None
      }
      case None => None
    }
  }

  def renameVariables(ref: TypingRule, rule: TypingRule): Option[TypingRule] = {
    var renamer = new VariableRenamer(ref, rule)
    try {
      val renaming = renamer()
      println(s"using renaming: $renaming")
      Some(TypingRule(rule.name,
        rule.premises.map(replaceMetaVars(_, renaming).asInstanceOf[TypingRuleJudgment]),
        rule.consequences.map(replaceMetaVars(_, renaming).asInstanceOf[TypingRuleJudgment])
      ))
    } catch {
      case a: RenamingError => {
        println(a)
        None
      }
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
