package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.backend.ast.{VeritasConstruct, VeritasFormula}

/**
  * specialized strategy that works on Veritas ASTs (SPL) and receives domain-specific knowledge
  * (DomainSpecificKnowledge, AugmentedCallGraph...) for specifications
  */
trait DomainSpecificStrategy extends Strategy[VeritasConstruct, VeritasFormula] {

}
