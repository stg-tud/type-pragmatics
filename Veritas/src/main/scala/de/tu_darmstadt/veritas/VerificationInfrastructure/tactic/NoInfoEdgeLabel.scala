package de.tu_darmstadt.veritas.VerificationInfrastructure.tactic

import de.tu_darmstadt.veritas.VerificationInfrastructure.EdgeLabel

object NoInfoEdgeLabel extends EdgeLabel {
  override def compare(that: EdgeLabel): Int = that match {
    case that: NoInfoEdgeLabel.type => 0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}
