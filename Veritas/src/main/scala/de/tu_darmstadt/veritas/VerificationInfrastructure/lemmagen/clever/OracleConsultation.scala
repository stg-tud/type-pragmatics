package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

trait OracleConsultation[Graph] {
  def consult(graph: Graph): Unit
}
