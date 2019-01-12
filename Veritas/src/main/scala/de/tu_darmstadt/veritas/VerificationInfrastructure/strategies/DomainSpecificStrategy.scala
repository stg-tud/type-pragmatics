package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

/**
  * specialized strategy that receives domain-specific knowledge
  * (DomainSpecificKnowledge, AugmentedCallGraph...) for specifications
  */
trait DomainSpecificStrategy[Spec, Goal, Type, FDef, Prop, Equation, Criteria, Expression] extends Strategy[Spec, Goal] {
  val dsk: DomainSpecificKnowledge[Type, FDef, Prop]
  val acg_gen: String => AugmentedCallGraph[Equation, Criteria, Expression]
}
