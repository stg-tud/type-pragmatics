package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.VerificationInfrastructure.domaindescription.DomainDescription
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.{StructInductCase, StructuralInduction}

case class DSStructuralInduction[Spec <: Ordered[Spec], Goal <: Ordered[Goal], Repr](spec: Spec, inductionvar: Spec, domaindescription: DomainDescription[Repr]) extends DSStrategy[Spec, Goal] {

  //TODO isApplicable is a candidate for a common strategy method
  /**
    * determines whether this strategy is applicable to a given goal
    */
  def isApplicable(g: Goal): Boolean = ???

  def extractUniversallyQuantifiedVars(g: Goal): Seq[Spec] = ???

  def extractPremises(g: Goal): Seq[Spec] = ???

  def extractConclusions(g: Goal): Seq[Spec] = ???

  override def applyToPG(pg: ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal])(obl: pg.Obligation): ProofGraph[Spec, Goal] = {
    //TODO: create the induction hypotheses, and the induction cases

    val goal = obl.goal
    if (isApplicable(goal)){

      val finalcases: Seq[(Goal, StructInductCase[Spec])] = ???
      pg.applyTactic(obl, StructuralInduction[Spec, Goal](spec, finalcases /*all the induction cases*/))
      pg
    }
    else
      pg //TODO throw an exception that explains why the strategy failed


  }
}
