package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

trait OracleConsultation {
  def consult(graph: RefinementGraph): Unit
}
