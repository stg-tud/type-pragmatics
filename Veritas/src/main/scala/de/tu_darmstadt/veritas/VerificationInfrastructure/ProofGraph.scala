package de.tu_darmstadt.veritas.VerificationInfrastructure

trait ProofGraph[Spec, Goal] {
  def allnodes: Iterable[ProofStep[Spec, Goal]]
  def nodes(status: VerificationStatus)

}