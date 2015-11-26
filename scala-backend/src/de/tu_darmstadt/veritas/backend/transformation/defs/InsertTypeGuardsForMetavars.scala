package de.tu_darmstadt.veritas.backend.transformation.defs

import de.tu_darmstadt.veritas.backend.util.FreshNames
import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.FunctionExpJudgment._
import de.tu_darmstadt.veritas.backend.transformation.TransformationError
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation
import de.tu_darmstadt.veritas.backend.veritas.function._
import de.tu_darmstadt.veritas.backend.Configuration
import de.tu_darmstadt.veritas.backend.transformation.CollectTypes
import de.tu_darmstadt.veritas.backend.tff.TffAtomicType

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
object InsertTypeGuardsForMetavars extends ModuleTransformation {

  private var types: CollectTypes = _

  override def transModule(name: String, is: Seq[Import], mdefs: Seq[ModuleDef]): Seq[Module] = {
    // collect types for current module
    types = new CollectTypes
    types.apply(Seq(Module(name, is, mdefs)))(config)
    
    super.transModule(name, is, mdefs)
  }

  override def transTypingRules(tr: TypingRule): Seq[TypingRule] = {
    withSuper(super.transTypingRules(tr)) {
      case tr@TypingRule(n, prems, conss) =>
        val varmap = types.inferMetavarTypes(tr)
        val guards = varmap map (kv => makeGuardPremise(kv._1, kv._2))
        Seq(TypingRule(n, guards.toSeq ++ prems, conss))
    }
  }
 
  private def makeGuardPremise(v: MetaVar, t: TffAtomicType): TypingRuleJudgment =
    FunctionExpJudgment(
        GenerateTypeGuards.guardCall(t.typename, FunctionMeta(v)))
}