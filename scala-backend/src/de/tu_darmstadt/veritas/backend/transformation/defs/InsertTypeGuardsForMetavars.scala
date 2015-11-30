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

/**
 * For each SortDef we generate a type guard and for each ConstructorDecl we generate an axiom for the guard.
 *
 * cons D : T
 * ==>
 * axiom $T(D)
 *
 * cons E : T * U -> V
 * ==>
 * axiom $T(x), $U(y) <-> $V(E(x,y))
 *
 * Also works with Local/Strategy blocks.
 */
object InsertTypeGuardsForMetavars extends ModuleTransformation with CollectTypes {

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    withSuper(super.transTypingRules(tr)) {
      case tr@TypingRule(n, prems, conss) =>
        val vars = inferMetavarTypes(tr)
        val guards = vars map (v => makeGuardPremise(v))
        Seq(TypingRule(n, guards.toSeq ++ prems, conss))
    }
  }
 
  private def makeGuardPremise(v: MetaVar): TypingRuleJudgment =
    FunctionExpJudgment(
        GenerateTypeGuards.guardCall(v.sortType.name, FunctionMeta(v)))
}