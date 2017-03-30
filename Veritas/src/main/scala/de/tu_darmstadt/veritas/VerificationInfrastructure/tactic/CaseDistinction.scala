package de.tu_darmstadt.veritas.VerificationInfrastructure.tactic

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenObligation, IProofGraph, ObligationProducer}

case class CaseDistinction[Spec, Goal]() extends Tactic[Spec, Goal] {

  override def compare(that: Tactic[Spec, Goal]): Int = that match {
    case that: CaseDistinction[Spec, Goal] => 0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }

  override def apply[Obligation](obl: GenObligation[Spec, Goal],
                                 obllabels: Iterable[EdgeLabel],
                                 produce: ObligationProducer[Spec, Goal, Obligation]): Iterable[(Obligation, EdgeLabel)] =
    ???
}
