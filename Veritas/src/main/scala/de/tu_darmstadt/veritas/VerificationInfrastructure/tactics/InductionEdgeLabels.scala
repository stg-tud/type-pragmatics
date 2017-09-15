package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure._
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.Verifier


// D: format for block of variable/constant declarations
case class FixedVars[D <: Ordered[D]](fixedvars: D) extends PropagatableInfo {
  override type P = D

  override def propagateInfo(): Option[D] = Some(fixedvars)
}

// A: format of axioms
case class InductionHypotheses[A <: Ordered[A]](ihs: A) extends PropagatableInfo {
  override type P = A

  override def propagateInfo(): Option[A] = Some(ihs)
}

/**
  *
  * @param casename  name of the induction case (should correspond to goal name of case?)
  * @param fixedvars variables that need to fixed so that they explicitly refer to the same variables in the ihs and in the goal
  * @param ihs       induction hypotheses
  */
case class StructInductCase[Defs <: Ordered[Defs], Formulae <: Defs with Ordered[Formulae]](casename: String, fixedvars: Option[FixedVars[Defs]],
                                                   ihs: InductionHypotheses[Formulae]) extends EdgeLabel {

  override def desc: String = casename

  override def propagateInfoList: Seq[PropagatableInfo] =
    fixedvars match {
      case Some(fv) => Seq(fv, ihs)
      case None => Seq(ihs)
    }

  override def compare(that: EdgeLabel): Int = that match {
    case that: StructInductCase[Defs, Formulae] =>
      val compare1 = this.casename compare that.casename
      if (compare1 != 0) return compare1
      val compare2 = this.ihs.ihs compare that.ihs.ihs
      if (compare2 != 0) return compare2
      0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

//TODO the information necessary for this edge might need to be refined
//TODO rethink the parameters of this EdgeLabel
case class CaseDistinctionCase[Defs <: Ordered[Defs], Formulae <: Defs with Ordered[Formulae]](casename: String,
                                                      fixedvars: Option[FixedVars[Defs]],
                                                      ihs: InductionHypotheses[Formulae]) extends EdgeLabel {
  override def desc: String = casename

  override def propagateInfoList: Seq[PropagatableInfo] =
    fixedvars match {
      case Some(fv) => Seq(fv, ihs)
      case None => Seq(ihs)
    }

  override def compare(that: EdgeLabel): Int = that match {
    case that: CaseDistinctionCase[Defs, Formulae] =>
      val compare1 = this.casename compare that.casename
      if (compare1 != 0) return compare1
      val compare2 = this.ihs.ihs compare that.ihs.ihs
      if (compare2 != 0) return compare2
      0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }

}

