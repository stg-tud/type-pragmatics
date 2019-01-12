package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies
import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}

/**
  * strategy that applies another strategy to all leaves reachable from the given obligation
  * @param s
  * @tparam Spec
  * @tparam Goal
  */
class ApplyStratToLeaves[Spec, Goal](s: Strategy[Spec, Goal]) extends Strategy[Spec, Goal]{
  override def applyToPG(pg: ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal])(obl: pg.Obligation): ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal] = {
    //look for all leaves reachable from the given obligation obl
    val reachable_leaves = pg.leaves(Set(obl))

    //apply given strategy to all leaves
    for (l <- reachable_leaves)
      s.applyToPG(pg)(l)

    pg
  }
}
