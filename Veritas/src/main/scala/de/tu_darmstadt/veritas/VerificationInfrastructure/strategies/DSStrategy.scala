package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals, Strategy}

/**
  * Domain-specific strategy (progress and preservation proofs)
  * A DSStrategy queries the specification of a language (including type system) via the DomainDescription interface.
  * From the query results, a DSStrategy constructs one or more nodes and proof steps with tactics within a proof graph
  *
  */
trait DSStrategy[Spec, Goal] extends Strategy[Spec, Goal]
//TODO Are there methods which all domain-specific strategies need?