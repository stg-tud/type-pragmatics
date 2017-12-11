package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.FunctionExpJudgment._
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.ast.function._
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectTypesDefs
import de.tu_darmstadt.veritas.backend.tff.TffAtomicType
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectTypesDefsClass

class InsertTypeGuardsForMetavars extends ModuleTransformation with CollectTypesDefs {

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    tr match {
      case tr @ TypingRule(n, prems, conss) if checkTypingRule(tr) =>
        {
          //infers types of all variables in the typing rule, also in quantifiers
          val vars = inferMetavarTypes(tr).keys
          val newprems = trace(prems)(transTypingRuleJudgments(_))
          val newconss = trace(conss)(transTypingRuleJudgments(_))
          val guards = vars map (v => makeGuardPremise(v))
          Seq(TypingRule(n, guards.toSeq ++ newprems, newconss))
        }
      case tr => Seq(tr)
    }
  }

  //generate guards in inner quantifiers as well
  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment = {
    withSuper(super.transTypingRuleJudgment(trj)) {
      case ExistsJudgment(vl, jl) => ExistsJudgment(vl, addGuardsToExistsBody(vl, jl))
      case ForallJudgment(vl, jl) => ForallJudgment(vl, addGuardsToForallBody(vl, jl))
    }
  }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] = {
    withSuper(super.transTypingRuleJudgments(trj)) {
      case ExistsJudgment(vl, jl) => Seq(ExistsJudgment(vl, addGuardsToExistsBody(vl, jl)))
      case ForallJudgment(vl, jl) => Seq(ForallJudgment(vl, addGuardsToForallBody(vl, jl)))
    }
  }

  def addGuardsToExistsBody(vl: Seq[MetaVar], jl: Seq[TypingRuleJudgment]): Seq[TypingRuleJudgment] = {
    //types of meta variables should have been inferred already
    //in exists bodies, guards must be attached via conjunction!
    val guards = vl map (v => makeGuardPremise(v))
    guards ++ jl
  }

  def addGuardsToForallBody(vl: Seq[MetaVar], jl: Seq[TypingRuleJudgment]): Seq[TypingRuleJudgment] = {
    //types of meta variables should have been inferred already
    val guards = vl map (v => makeGuardPremise(v))
    val negguards = guards map (g => NotJudgment(g))
    Seq(OrJudgment(Seq(negguards, jl)))
  }

  private def makeGuardPremise(v: MetaVar): TypingRuleJudgment =
    FunctionExpJudgment(
      GenerateAllTypeGuards.guardCall(v.sortType.name, FunctionMeta(v)))

  def checkTypingRule(tr: TypingRule): Boolean =
    tr match {
      case TypingRule(n, _, _) => !(n.startsWith(GenerateAllTypeGuards.ruleprefix))
    }
}

object InsertTypeGuardsForAllMetavars extends InsertTypeGuardsForMetavars
