package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.specqueries.SpecEnquirer
import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

/**
  * specialized strategy that receives domain-specific knowledge
  * (DomainSpecificKnowledge, AugmentedCallGraph...) for specifications
  */
trait DomainSpecificStrategy[Def,
  Formulae <: Def,
  Type <: Def,
  FDef <: Def,
  Prop <: Formulae,
  Equation <: Def,
  Criteria <: Def,
  Expression <: Def] extends Strategy[Def, Formulae] {
  val dsk: DomainSpecificKnowledge[Type, FDef, Prop]
  val acg_gen: String => AugmentedCallGraph[Equation, Criteria, Expression]
  val spec_enquirer: SpecEnquirer[Def, Formulae]
}
