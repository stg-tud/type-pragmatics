package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

/**
  * iterative strategy - builds a proof graph according to the "template" given by the augmented call graph
  * assumes a proof graph that already contains nodes
  * @param dsk domain-specific knowledge for the given specification
  * @param acg_gen function that can generate an augmented call graph for a given function name
  */
case class ProgressPreservationBasicLoop[Spec, Goal, Type, FDef, Prop, Equation, Criteria, Expression](override val dsk: DomainSpecificKnowledge[Type, FDef, Prop], override val acg_gen: String => AugmentedCallGraph[Equation, Criteria, Expression])
  extends DomainSpecificStrategy[Spec, Goal, Type, FDef, Prop, Equation, Criteria, Expression] {


  override def applyToPG(pg: ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal])(obl: pg.Obligation):
    ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal] = {

    //TODO: iteratively traverse given acg; build proof graph according to the ACG structure

    //intermediate step: apply Solve tactic to all leaves
    ApplySolveToLeaves().applyToPG(pg)(obl)


    pg
  }
}
