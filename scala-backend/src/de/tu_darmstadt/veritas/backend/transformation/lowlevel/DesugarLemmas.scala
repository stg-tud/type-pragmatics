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
 */
object DesugarLemmas extends VeritasConstructTransformation {
  override def transform: PartialFunction[VeritasConstruct, Seq[VeritasConstruct]] = {
    case Lemmas(lm, t)                => Seq(Axioms(lm), Goals(lm, t))
    case LemmasWithStrategy(s, lm, t) => Seq(Axioms(lm), GoalsWithStrategy(s, lm, t))
  }
}