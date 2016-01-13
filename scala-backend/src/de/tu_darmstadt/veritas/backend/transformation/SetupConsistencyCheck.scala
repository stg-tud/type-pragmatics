package de.tu_darmstadt.veritas.backend.transformation

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.veritas.function._

/**
 * assumes that module is already in a state in which it could be translated to fof/tff
 * attaches a false goal for consistency checking
 */
object SetupConsistencyCheck extends ModuleTransformation {

  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] =
    withSuper(super.transModuleDefs(mdef)) {
      // substitute actual goal with a false goal - shall only check axioms!
      case Goals(gs, t) => Seq(Goals(Seq(TypingRule("false-goal", Seq(), Seq(FunctionExpJudgment(FunctionExpFalse)))), None))
    }
}