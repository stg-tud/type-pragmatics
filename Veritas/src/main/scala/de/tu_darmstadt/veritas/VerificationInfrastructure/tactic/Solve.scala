package de.tu_darmstadt.veritas.VerificationInfrastructure.tactic

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, GenObligation, IProofGraph}

/**
  * default tactic: simply try to figure out a proof for a node of a proof graph
  * given its children, e.g. calling an ATP
  */

case class Solve[Spec, Goal]() extends Tactic[Spec, Goal] {
  override def compare(that: Tactic[Spec, Goal]): Int = that match {
    case that: Solve[Spec, Goal] => 0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
   }

  /* applying the Solve tactic does not generate any edges */
  def apply(g: IProofGraph[Spec, Goal])(obl: g.Obligation): Iterable[(g.Obligation, EdgeLabel)] = Seq()
}
