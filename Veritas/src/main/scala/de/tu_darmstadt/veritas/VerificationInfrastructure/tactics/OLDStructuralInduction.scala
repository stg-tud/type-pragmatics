package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure._
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.Verifier

case class OLDStructuralInduction[Spec <: Ordered[Spec], Goal <: Ordered[Goal]](spec: Spec, cases: Seq[(Goal, StructInductCase[Spec])]) extends Tactic[Spec, Goal] {
  //TODO we might have to refine the verifier call for induction once we really support this via a prover
  override def verifyStep[Result <: GenStepResult[Spec, Goal]](obl: GenObligation[Spec, Goal],
                                                               parentedges: Iterable[EdgeLabel],
                                                               subobl: Iterable[GenObligation[Spec, Goal]],
                                                               verifier: Verifier[Spec, Goal],
                                                               produce: StepResultProducer[Spec, Goal, Result],
                                                               pathforlogs: Option[String] = None): Result =
    super.verifyStep(obl, parentedges, subobl, verifier, produce)


  override def compare(that: Tactic[Spec, Goal]): Int = that match {
    case that: OLDStructuralInduction[Spec, Goal] => {
      val caseslengthcomp = this.cases.length compare that.cases.length
      if (caseslengthcomp == 0) {
        //compare individual cases against each other until first difference found
        lazy val individualcomp = for (((s1, e1), (s2, e2)) <- this.cases zip that.cases) yield (s1 compare s2, e1 compare e2)
        val comppair = individualcomp.find(_ != (0, 0)).getOrElse((0, 0))
        if (comppair._1 == 0) comppair._2 else comppair._1
      }
      else
        caseslengthcomp

    }
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }

  override def apply[Obligation](obl: GenObligation[Spec, Goal],
                                 obllabels: Iterable[EdgeLabel],
                                 produce: ObligationProducer[Spec, Goal, Obligation]): Iterable[(Obligation, EdgeLabel)] =
    for ((sobl, edge) <- cases) yield {
      val newobl = produce.newObligation(spec, sobl)
      (newobl, edge)
    }
}

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
case class StructInductCase[Spec <: Ordered[Spec]](casename: String, fixedvars: Option[FixedVars[Spec]],
                                                   ihs: InductionHypotheses[Spec]) extends EdgeLabel {

  override def desc: String = casename

  override def propagateInfoList: Seq[PropagatableInfo] =
    fixedvars match {
      case Some(fv) => Seq(fv, ihs)
      case None => Seq(ihs)
    }

  override def compare(that: EdgeLabel): Int = that match {
    case that: StructInductCase[Spec] =>
      val compare1 = this.casename compare that.casename
      if (compare1 != 0) return compare1
      val compare2 = this.ihs.ihs compare that.ihs.ihs
      if (compare2 != 0) return compare2
      0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

//TODO the information necessary for this edge might need to be refined
case class CaseDistinctionCase[Spec <: Ordered[Spec]](casename: String,
                                                      fixedvars: Option[FixedVars[Spec]],
                                                      ihs: InductionHypotheses[Spec]) extends EdgeLabel {
  override def desc: String = casename

  override def propagateInfoList: Seq[PropagatableInfo] =
    fixedvars match {
      case Some(fv) => Seq(fv, ihs)
      case None => Seq(ihs)
    }

  override def compare(that: EdgeLabel): Int = that match {
    case that: StructInductCase[Spec] =>
      val compare1 = this.casename compare that.casename
      if (compare1 != 0) return compare1
      val compare2 = this.ihs.ihs compare that.ihs.ihs
      if (compare2 != 0) return compare2
      0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }

}

//TODO edge information for lemma applications might have to be refined
// IDEAS: possible lemma instantiations, application hints (order...?)
case class LemmaApplication[Goal <: Ordered[Goal]](lemmaname: String) extends EdgeLabel {
  override def desc: String = lemmaname

  override def propagateInfoList: Seq[PropagatableInfo] = Seq()

  override def compare(that: EdgeLabel): Int = ???
}
