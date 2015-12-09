package de.tu_darmstadt.veritas.backend.transformation.defs

import scala.collection.mutable.Map
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.util.FreeVariables
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectTypes

/**
 * generates smaller inversion lemmas for separate cases, removing OR-constructs from inversion lemmas.
 * assumes that an inversion lemma based on function axioms has already been generated.
 *
 */
trait FunctionInversionAxiomSplit extends ModuleTransformation {

  private def nameOf(metaVar: MetaVar) =
    metaVar match {
      case MetaVar(name) => name
    }

  private def occursIn(term: Seq[Seq[VeritasConstruct]])(metaVar: MetaVar): Boolean = {
    term exists { y =>
      y exists { x =>
        x match {
          case FunctionMeta(MetaVar(name)) => name == nameOf(metaVar)
          case e => occursIn(e.children)(metaVar)
        }
      }
    }
  }

  private def removeUsedVariables(varlist: Seq[MetaVar], term: TypingRuleJudgment): Seq[MetaVar] = {
    varlist filter {x => !occursIn(term.children)(x)}
  }

  private def splitOrCase(orcase: Seq[TypingRuleJudgment]): (Seq[TypingRuleJudgment], Seq[TypingRuleJudgment]) = {
    if (orcase.length > 1) {
      (Seq(orcase.last), orcase.dropRight(1))
    } else {
      orcase match {
        case Seq(ExistsJudgment(varlist, judgeList)) => {
          val (newPremises, consequences) = splitOrCase(judgeList)
          val leftOverVariables = removeUsedVariables(varlist, newPremises.head)

          (newPremises, Seq(ExistsJudgment(leftOverVariables, consequences)))
        }
        case _ => ???
      }
    }
  }

  private def splitOrCases(orcases: Seq[Seq[TypingRuleJudgment]]): Seq[(Seq[TypingRuleJudgment], Seq[TypingRuleJudgment])] = ???

  private def createInversionAxioms(name: String, premises: Seq[TypingRuleJudgment], orcases: Seq[Seq[TypingRuleJudgment]]) = {
    var counter = 0

    orcases map { x =>
      {
        val (newPremises, newConsequences) = splitOrCase(x)
        counter += 1
        TypingRule(name + counter, premises ++ newPremises, newConsequences)
      }
    }
  }

  private def transformInversionAxiom(t: TypingRule): Seq[TypingRule] = {
    t.consequences match {
      case Seq(OrJudgment(orcases)) => createInversionAxioms(t.name, t.premises, orcases)
      case Seq(_)                   => Seq(t)
    }
  }

  private def isInversionLemma(t: TypingRule) =
    t.name.endsWith("-INV") && !t.name.endsWith("false-INV") && !t.name.endsWith("true-INV")

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    withSuper(super.transModuleDefs(mdef)) {
      case as @ Axioms(tseq) => Seq(as, Axioms(tseq flatMap { t =>
        {
          if (isInversionLemma(t))
            transformInversionAxiom(t)
          else
            Seq(t)
        }
      }))
    }
}

object NoBooleanFunctionInversionAxiomSplit extends FunctionInversionAxiomSplit