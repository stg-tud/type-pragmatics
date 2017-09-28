package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure._
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.Verifier


// D: format for single variable/constant
case class FixedVar[D](fixedvar: D) extends PropagatableInfo {
  override type P = D

  override def propagateInfo(): D = fixedvar
}

// A: format of single axiom
case class InductionHypothesis[A](ih: A) extends PropagatableInfo {
  override type P = A

  override def propagateInfo(): A = ih
}

/**
  *
  * @param casename  name of the induction case (should correspond to goal name of case?)
  * @param fixedvars variables that need to fixed so that they explicitly refer to the same variables in the ihs and in the goal
  * @param ihs       induction hypotheses
  */
case class StructInductCase[Defs, Formulae <: Defs](casename: String, fixedvars: Seq[FixedVar[Defs]], ihs: Seq[InductionHypothesis[Formulae]], propInfo: Seq[PropagatableInfo]) extends EdgeLabel {

  override def desc: String = casename

  override def propagateInfoList: Seq[PropagatableInfo] =
    propInfo ++ fixedvars ++ ihs

  override def compare(that: EdgeLabel): Int = that match {
    case that: StructInductCase[Defs, Formulae] =>
      this.casename compare that.casename //for now assume that case names are unique!

    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

//TODO the information necessary for this edge might need to be refined
//TODO rethink the parameters of this EdgeLabel
case class CaseDistinctionCase[Defs, Formulae <: Defs](casename: String, propInfo: Seq[PropagatableInfo]) extends EdgeLabel {
  override def desc: String = casename

  override def propagateInfoList: Seq[PropagatableInfo] = propInfo

  override def compare(that: EdgeLabel): Int = that match {
    case that: StructInductCase[Defs, Formulae] =>
      this.casename compare that.casename //for now assume that case names are unique!

    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }

}

