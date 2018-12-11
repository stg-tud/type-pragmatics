package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}

/**
  * top-level strategy for initializing a proof graph, given a specification
  */
trait InitializationStrategy[Spec, Goal] {
  def initializePG(s: Spec): ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal]
}
