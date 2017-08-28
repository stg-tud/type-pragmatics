package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.VerificationInfrastructure.domaindescription.DomainDescription
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.StructuralInduction

case class DSStructuralInduction[Spec <: Ordered[Spec], Goal <: Ordered[Goal], Repr](spec: Spec, inductionvar : Spec, domaindescription: DomainDescription[Repr]) extends DSStrategy[Spec, Goal] {

  override def applyToPG(pg: ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal])(obl: pg.Obligation): ProofGraph[Spec, Goal] = {
    //TODO: create the induction hypotheses, and the induction cases
    pg.applyTactic(obl, StructuralInduction[Spec, Goal](spec, ??? /*all the induction cases*/))
    pg
  }
}
