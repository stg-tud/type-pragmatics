package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * A Strategy takes a graph and modifies it (e.g. via applying tactics and
  * inspecting the result) and returns the new graph
  */
trait Strategy[Spec, Goal] {
  def applyToPG(pg: ProofGraph[Spec, Goal])(obl: pg.Obligation): ProofGraph[Spec, Goal]
}
