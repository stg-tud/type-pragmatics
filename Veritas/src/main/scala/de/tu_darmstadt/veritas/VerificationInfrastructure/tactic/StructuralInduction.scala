package de.tu_darmstadt.veritas.VerificationInfrastructure.tactic

import de.tu_darmstadt.veritas.VerificationInfrastructure.ProofGraph.ProofEdges
import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, Obligation, StepResult, Verifier}

case class StructuralInduction[Spec <: Ordered[Spec], Goal <: Ordered[Goal]](inductionvar: Spec) extends Tactic[Spec, Goal] {
  //TODO we might have to refine the verifier call for induction once we really support this via a prover
  override def verifyStep(step: Obligation[Spec, Goal], edges: ProofEdges[Spec, Goal], verifier: Verifier[Spec, Goal]): StepResult[Spec, Goal] = super.verifyStep(step, edges, verifier)


  override def compare(that: Tactic[Spec, Goal]): Int = that match {
    case that: StructuralInduction[Spec, Goal] => this.inductionvar compare that.inductionvar
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }

  override def apply(obl: Obligation[Spec, Goal]): Iterable[(Obligation[Spec, Goal], EdgeLabel)] = ???
}

/**
  *
  * @param casename name of the induction case (should correspond to goal name of case?)
  * @param ihs induction hypotheses
  * @tparam Goal type of the format for defining properties/goals
  */
case class StructInductCase[Goal <: Ordered[Goal]](casename: String, ihs: Seq[Goal]) extends EdgeLabel {
  override def compare(that: EdgeLabel): Int = that match {
    case that: StructInductCase[Goal] =>
      val compare1 = this.casename compare that.casename
      if (compare1 != 0) return compare1
      val compare2 = this.ihs.size compare that.ihs.size
      if (compare2 != 0) return compare2
      val compare3 = this.ihs.zip(that.ihs).foreach { ih =>
        val compared = ih._1 compare ih._2
        if (compared != 0)
          return compared
      }
      0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}
