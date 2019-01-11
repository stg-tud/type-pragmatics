package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.Solve
import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}

/**
  * strategy that applies the Solve tactic to the given obligation
  * @tparam Spec
  * @tparam Goal
  */
case class ApplySolve[Spec, Goal]() extends Strategy[Spec, Goal] {
  override def applyToPG(pg: ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal])(obl: pg.Obligation): ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal] = {
    pg. applyTactic(obl, Solve[Spec, Goal]())
    pg
  }
}

/**
  * Strategy that takes a proof graph and applies solve to all leaves of the proof graph
  * (overriding previously existing tactics)
  * @tparam Spec
  * @tparam Goal
  */
case class ApplySolveToLeaves[Spec, Goal]() extends ApplyStratToLeaves[Spec, Goal](ApplySolve())
