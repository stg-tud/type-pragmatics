package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas._
import de.tu_darmstadt.veritas.backend.transformation.ModuleTransformation

/**
 * no precondition necessary,
 * simply desugars lemmas to Axioms and Goals
 * - the order is important: first goal, then axiom!
 */
object DesugarLemmas extends ModuleTransformation {
  override def transModuleDefs(mdef: ModuleDef): Seq[ModuleDef] = mdef match {
    case Lemmas(lm, t)                => Seq(Goals(lm, t), Axioms(lm))
    case LemmasWithStrategy(s, lm, t) => Seq(GoalsWithStrategy(s, lm, t), Axioms(lm))
    case s                            => super.transModuleDefs(mdef)
  }

  //  override def transform: PartialFunction[VeritasConstruct, Seq[VeritasConstruct]] = {
  //    case Lemmas(lm, t)                => Seq(Goals(lm, t), Axioms(lm))
  //    case LemmasWithStrategy(s, lm, t) => Seq(GoalsWithStrategy(s, lm, t), Axioms(lm))
  //  }
}