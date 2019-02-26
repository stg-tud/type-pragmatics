package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.oracle

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.RefinementGraph

trait OracleConsultation {
  def consult(graph: RefinementGraph): Unit
}
