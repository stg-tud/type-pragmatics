package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

trait LemmaSelectionStrategy[Type, FDef, Prop, Equation, Criteria, Expression] {

  def selectLemma(dsk: DomainSpecificKnowledge[Type, FDef, Prop], acg: AugmentedCallGraph[Equation, Criteria, Expression], fn: String): Seq[Prop]

}
