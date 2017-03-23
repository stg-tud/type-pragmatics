package de.tu_darmstadt.veritas.VerificationInfrastructure.tactic

import de.tu_darmstadt.veritas.VerificationInfrastructure._
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.Verifier

case class StructuralInduction[Spec <: Ordered[Spec], Goal <: Ordered[Goal]](inductionvar: Spec) extends Tactic[Spec, Goal] {
  //TODO we might have to refine the verifier call for induction once we really support this via a prover
  override def verifyStep[Result](obl: GenObligation[Spec, Goal], edges: Iterable[(GenObligation[Spec, Goal], EdgeLabel)], verifier: Verifier[Spec, Goal], produce: StepResultProducer[Spec, Goal, Result]): Result =
    super.verifyStep(obl, edges, verifier, produce)


  override def compare(that: Tactic[Spec, Goal]): Int = that match {
    case that: StructuralInduction[Spec, Goal] => this.inductionvar compare that.inductionvar
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }

  override def apply[Obligation](obl: GenObligation[Spec, Goal], produce: ObligationProducer[Spec, Goal, Obligation]): Iterable[(Obligation, EdgeLabel)] =
    ???
}

/**
  *
  * @param casename name of the induction case (should correspond to goal name of case?)
  * @param fixedvars variables that need to fixed so that they explicitly refer to the same variables in the ihs and in the goal
  * @param ihs induction hypotheses
  * @tparam Goal type of the format for defining properties/goals
  */
case class StructInductCase[Spec <: Ordered[Spec], Goal <: Ordered[Goal]](casename: String, fixedvars: Option[Spec], ihs: Seq[Goal]) extends EdgeLabel {
  override def compare(that: EdgeLabel): Int = that match {
    case that: StructInductCase[Spec, Goal] =>
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

//TODO the information necessary for this edge might need to be refined
case class CaseDistinctionCase[Spec <: Ordered[Spec], Goal <: Ordered[Goal]](casename: String, fixedvars: Option[Spec], ihs: Seq[Goal]) extends EdgeLabel {
  override def compare(that: EdgeLabel): Int = ???
}
