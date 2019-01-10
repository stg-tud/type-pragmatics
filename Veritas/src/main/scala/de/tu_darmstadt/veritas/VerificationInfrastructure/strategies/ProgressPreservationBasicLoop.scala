package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import java.io.File

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals, ProofGraphUI, ProofGraphXodus}
import de.tu_darmstadt.veritas.backend.ast.function.{FunctionEq, FunctionExp, FunctionExpMeta}
import de.tu_darmstadt.veritas.backend.ast.{VeritasConstruct, VeritasFormula}
import de.tu_darmstadt.veritas.scalaspl.dsk.DomainSpecificKnowledge
import de.tu_darmstadt.veritas.scalaspl.util.{AugmentedCallGraph, VeritasAugmentedCallGraph}

/**
  * iterative strategy - builds a proof graph according to the "template" given by the augmented call graph
  * @param dsk domain-specific knowlegde for the given specification
  * @param acg augmented call graph from reduce function on - dsk and acg have to be from the SAME specification, of course!
  */
case class ProgressPreservationBasicLoop(dsk: DomainSpecificKnowledge, acg: AugmentedCallGraph[FunctionEq, FunctionExp, FunctionExpMeta]) extends DomainSpecificStrategy(dsk, acg) {


  override def applyToPG(pg: ProofGraph[VeritasConstruct, VeritasFormula] with ProofGraphTraversals[VeritasConstruct, VeritasFormula])(obl: pg.Obligation):
    ProofGraph[VeritasConstruct, VeritasFormula] with ProofGraphTraversals[VeritasConstruct, VeritasFormula] = {

    //TODO: iteratively traverse given acg; build proof graph according to the ACG structure

    //intermediate step: apply Solve tactic to all leaves
    ApplySolveToLeaves().applyToPG(pg)(obl)


    pg
  }
}
