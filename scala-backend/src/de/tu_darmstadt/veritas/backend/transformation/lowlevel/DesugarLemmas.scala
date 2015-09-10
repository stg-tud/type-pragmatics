package de.tu_darmstadt.veritas.backend.transformation.lowlevel

import de.tu_darmstadt.veritas.backend.veritas.ModuleDef
import de.tu_darmstadt.veritas.backend.veritas.Axioms
import de.tu_darmstadt.veritas.backend.veritas.Lemmas
import de.tu_darmstadt.veritas.backend.veritas.Goals
import de.tu_darmstadt.veritas.backend.veritas.LemmasWithStrategy
import de.tu_darmstadt.veritas.backend.veritas.GoalsWithStrategy
import de.tu_darmstadt.veritas.backend.transformation.ModuleDefTransformation

/**
 * no precondition necessary, 
 * simply desugars lemmas to Axioms and Goals
 */
object DesugarLemmas extends ModuleDefTransformation {
  override protected def apply: PartialFunction[ModuleDef, Seq[ModuleDef]] = {
    case Lemmas(lm, t)                => Seq(Axioms(lm), Goals(lm, t))
    case LemmasWithStrategy(s, lm, t) => Seq(Axioms(lm), GoalsWithStrategy(s, lm, t))
  }
}