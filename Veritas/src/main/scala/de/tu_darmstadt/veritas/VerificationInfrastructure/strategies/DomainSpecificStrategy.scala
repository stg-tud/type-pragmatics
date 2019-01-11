package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.backend.ast.function.{FunctionEq, FunctionExp, FunctionExpMeta}
import de.tu_darmstadt.veritas.backend.ast.{VeritasConstruct, VeritasFormula}
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledge
import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

/**
  * specialized strategy that works on Veritas ASTs (SPL) and receives domain-specific knowledge
  * (DomainSpecificKnowledge, AugmentedCallGraph...) for specifications
  */
abstract class DomainSpecificStrategy(dsk: DomainSpecificKnowledge, acg: AugmentedCallGraph[FunctionEq, FunctionExp, FunctionExpMeta]) extends Strategy[VeritasConstruct, VeritasFormula] {

}
