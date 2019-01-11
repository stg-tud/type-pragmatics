package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies
import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}

/**
  * apply a given strategy to all roots of a proof graph (assumption: root node are all nodes stored under a name in the proof graph)
  * given obl is irrelevant
  * @param s
  * @tparam Spec
  * @tparam Goal
  */
class ApplyStratToRoots[Spec, Goal](s: Strategy[Spec, Goal]) extends Strategy[Spec, Goal]{
  override def applyToPG(pg: ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal])(obl: pg.Obligation): ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal] = {
    val roots = pg.storedObligations.values

    for (r <- roots)
      s.applyToPG(pg)(r)

    pg
  }
}
