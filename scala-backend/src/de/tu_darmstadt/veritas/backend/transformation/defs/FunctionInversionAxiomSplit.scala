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

  private def removeUsedVariables(varlist: Seq[MetaVar], term: TypingRuleJudgment): Seq[MetaVar] = {
    val usedVariables = collectVariablesInExpression(term)
    varlist filter { x => !usedVariables.exists { y => x.name == y} }
  }

  private def splitOrCase(orcase: Seq[TypingRuleJudgment], parameters: Seq[String]): (Seq[TypingRuleJudgment], Seq[TypingRuleJudgment]) = {
    if (orcase.length > 1) {
      orcase partition { judgement =>
        {
          val occuringVariables = collectVariablesInExpression(judgement)
          !occuringVariables.exists { v => parameters.contains(v) }
        }
      }
    } else {
      orcase match {
        case Seq(ExistsJudgment(varlist, judgeList)) => {
          val (newPremises, consequences) = splitOrCase(judgeList, parameters)
          val leftOverVariables = newPremises.foldLeft(varlist)((newVarList, premise) => {
            removeUsedVariables(newVarList, premise)
          })
          //val leftOverVariables = removeUsedVariables(varlist, newPremises.head)

          (newPremises, Seq(ExistsJudgment(leftOverVariables, consequences)))
        }
        case _ => ???
      }
    }
  }

  private def splitOrCases(orcases: Seq[Seq[TypingRuleJudgment]]): Seq[(Seq[TypingRuleJudgment], Seq[TypingRuleJudgment])] = ???

  private def collectVariablesInExpression(exp: VeritasConstruct): Seq[String] = {
    exp.children flatMap { y =>
      y flatMap { x =>
        x match {
          case MetaVar(name) => Seq(name)
          case e             => collectVariablesInExpression(e)
        }
      }
    }
  }

  private def findFunctionParameterNames(f1: FunctionExpMeta) = f1 match {
    case FunctionExpApp(_, parameterList) => parameterList flatMap collectVariablesInExpression
    case _ => Seq("") //TODO think about what should happen if first case does not match - throw exception?
  }
  private def findResultVariableName(f2: FunctionExpMeta) = f2 match {
    case FunctionMeta(MetaVar(name)) => name
    case _ => "" //TODO think about what should happen if first case does not match - throw exception?
  }

  private def variableNames(premises: Seq[TypingRuleJudgment]): (String, Seq[String]) = {
    premises match {
      case Seq(FunctionExpJudgment(FunctionExpEq(f1, f2))) => {
        val resultVariable = findResultVariableName(f2)
        val functionParameters = findFunctionParameterNames(f1)
        (resultVariable, functionParameters)
      }
    }
  }

  private def createInversionAxioms(name: String, premises: Seq[TypingRuleJudgment], orcases: Seq[Seq[TypingRuleJudgment]]) = {
    var counter = 0
    val (result, parameters) = variableNames(premises)
    orcases map { x =>
      {
        val (newPremises, newConsequences) = splitOrCase(x, parameters)
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
      case Axioms(tseq) => Seq(Axioms(tseq flatMap { t =>
        {
          if (isInversionLemma(t))
            t +: transformInversionAxiom(t)
          else
            Seq(t)
        }
      }))
    }
}

object NoBooleanFunctionInversionAxiomSplit extends FunctionInversionAxiomSplit