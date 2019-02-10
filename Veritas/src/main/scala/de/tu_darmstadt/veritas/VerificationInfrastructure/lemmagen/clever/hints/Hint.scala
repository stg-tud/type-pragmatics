package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.hints

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.Lemma
import de.tu_darmstadt.veritas.backend.ast.MetaVar

trait Hint {
  def apply(lemma: Lemma,
            postVariables: Set[MetaVar],
            constrainedVariables: Set[MetaVar]): (Lemma, Set[MetaVar], Set[MetaVar])
}
