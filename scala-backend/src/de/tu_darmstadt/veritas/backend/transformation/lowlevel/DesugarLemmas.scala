package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas.VeritasConstruct
import de.tu_darmstadt.veritas.backend.veritas.Axioms
import de.tu_darmstadt.veritas.backend.veritas.Lemmas
import de.tu_darmstadt.veritas.backend.veritas.Goals
import de.tu_darmstadt.veritas.backend.veritas.LemmasWithStrategy
import de.tu_darmstadt.veritas.backend.veritas.GoalsWithStrategy
import de.tu_darmstadt.veritas.backend.transformation.VeritasConstructTransformation

/**
 * no precondition necessary, 
 * simply desugars lemmas to Axioms and Goals
 * - the order is important: first goal, then axiom!
 */
object DesugarLemmas extends VeritasConstructTransformation {
  override def transform: PartialFunction[VeritasConstruct, Seq[VeritasConstruct]] = {
    case Lemmas(lm, t)                => Seq(Goals(lm, t), Axioms(lm))
    case LemmasWithStrategy(s, lm, t) => Seq(GoalsWithStrategy(s, lm, t), Axioms(lm))
  }
}