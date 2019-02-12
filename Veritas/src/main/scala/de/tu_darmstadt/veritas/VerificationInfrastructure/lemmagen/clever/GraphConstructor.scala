package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

trait GraphConstructor[Graph] {
  def construct(): Graph
}
