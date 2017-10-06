package de.tu_darmstadt.veritas.VerificationInfrastructure.tactics

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, PropagatableInfo}

object NoInfoEdgeLabel extends EdgeLabel {

  override def desc: String = ""

  override def propagateInfoList: Seq[PropagatableInfo] = Seq()

}
