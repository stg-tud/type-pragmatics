package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.VerificationInfrastructure.domaindescription.DomainDescription

case class DSStructuralInduction[Spec, Goal, I, Repr](inductionvar : I, domaindescription: DomainDescription[Repr]) extends DSStrategy[Spec, Goal] {

  override def applyToPG(pg: ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal])(obl: pg.Obligation): ProofGraph[Spec, Goal] = ???
}
