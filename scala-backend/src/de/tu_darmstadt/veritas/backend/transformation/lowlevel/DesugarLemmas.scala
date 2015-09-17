package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleDefTransformation

/**
 * no precondition necessary, 
 * simply desugars lemmas to Axioms and Goals
 * - the order is important: first goal, then axiom!
 */
object DesugarLemmas extends ModuleDefTransformation {
  override def apply: PartialFunction[ModuleDef, Seq[ModuleDef]] = {
    case Lemmas(lm, t)                => Seq(Goals(lm, t), Axioms(lm))
    case LemmasWithStrategy(s, lm, t) => Seq(GoalsWithStrategy(s, lm, t), Axioms(lm))
  }
}