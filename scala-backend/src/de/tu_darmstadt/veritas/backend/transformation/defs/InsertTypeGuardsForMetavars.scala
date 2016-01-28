package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpJudgment._
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectTypes
import de.tu_darmstadt.veritas.backend.tff.TffAtomicType
import de.tu_darmstadt.veritas.backend.transformation.collect.CollectTypesClass

object InsertTypeGuardsForMetavars extends ModuleTransformation with CollectTypes {

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    tr match {
      case tr @ TypingRule(n, prems, conss) =>
        if (n.startsWith(GenerateTypeGuards.ruleprefix))
          Seq(tr)
        else {
          //infers types of all variables in the typing rule, also in quantifiers
          val vars = inferMetavarTypes(tr)
          val newprems = trace(prems)(transTypingRuleJudgments(_))
          val newconss = trace(conss)(transTypingRuleJudgments(_))
          val guards = vars map (v => makeGuardPremise(v))
          Seq(TypingRule(n, guards.toSeq ++ newprems, newconss))
        }
    }
  }

  //generate guards in inner quantifiers as well
  override def transTypingRuleJudgment(trj: TypingRuleJudgment): TypingRuleJudgment = {
    withSuper(super.transTypingRuleJudgment(trj)) {
      case ExistsJudgment(vl, jl) => ExistsJudgment(vl, addGuardsToQuantifierBody(vl, jl))
      case ForallJudgment(vl, jl) => ForallJudgment(vl, addGuardsToQuantifierBody(vl, jl))
    }
  }

  override def transTypingRuleJudgments(trj: TypingRuleJudgment): Seq[TypingRuleJudgment] = {
    withSuper(super.transTypingRuleJudgments(trj)) {
      case ExistsJudgment(vl, jl) => Seq(ExistsJudgment(vl, addGuardsToQuantifierBody(vl, jl)))
      case ForallJudgment(vl, jl) => Seq(ForallJudgment(vl, addGuardsToQuantifierBody(vl, jl)))
    }
  }

  private def addGuardsToQuantifierBody(vl: Seq[MetaVar], jl: Seq[TypingRuleJudgment]): Seq[TypingRuleJudgment] = {
    //types of meta variables should have been inferred already
    val guards = vl map (v => makeGuardPremise(v))
    val negguards = guards map (g => NotJudgment(g))
    Seq(OrJudgment(Seq(negguards, jl)))
  }

  private def makeGuardPremise(v: MetaVar): TypingRuleJudgment =
    FunctionExpJudgment(
      GenerateTypeGuards.guardCall(v.sortType.name, FunctionMeta(v)))
}