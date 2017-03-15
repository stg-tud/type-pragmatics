package de.tu_darmstadt.veritas.VerificationInfrastructure.tactic

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, Obligation}
import de.tu_darmstadt.veritas.VerificationInfrastructure.ProofGraph.ProofEdges

case class CaseDistinction[Spec, Goal]() extends Tactic[Spec, Goal] {

  override def compare(that: Tactic[Spec, Goal]): Int = that match {
    case that: CaseDistinction[Spec, Goal] => 0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }

  override def apply(obl: Obligation[Spec, Goal]): Iterable[(Obligation[Spec, Goal], EdgeLabel)] = ???
}
