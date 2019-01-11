package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.Solve
import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}


/**
  * Strategy that takes a proof graph and applies solve to all leaves of the proof graph
  * (overriding previously existing tactics)
  * @tparam Spec
  * @tparam Goal
  */
case class ApplySolveToLeaves[Spec, Goal]() extends Strategy[Spec, Goal]{
  override def applyToPG(pg: ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal])(obl: pg.Obligation): ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal] = {
    //look for all leaves reachable from the given obligation obl
    val reachable_leaves = pg.leaves(Set(obl))

    //apply Solve tactic to all leaves
    for (l <- reachable_leaves) {
      pg.applyTactic(l, Solve[Spec, Goal]())
    }

    pg
  }
}
