package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenObligation, ObligationProducer}

/**
  * default tactic: simply try to figure out a proof for a node of a proof graph
  * given its children, e.g. calling an ATP
  */

case class Solve[Spec, Goal]() extends Tactic[Spec, Goal] {
  /* applying the Solve tactic does not generate any edges */
  def apply[Obligation](obl: GenObligation[Spec, Goal],
                        obllabels: Iterable[EdgeLabel],
                        produce: ObligationProducer[Spec, Goal, Obligation]): Iterable[(Obligation, EdgeLabel)] =
    Seq()


}
