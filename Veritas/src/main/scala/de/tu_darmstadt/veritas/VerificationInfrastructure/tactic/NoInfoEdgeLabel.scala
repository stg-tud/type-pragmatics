package de.tu_darmstadt.veritas.VerificationInfrastructure.tactic

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, PropagatableInfo}

object NoInfoEdgeLabel extends EdgeLabel with Serializable {
  override def compare(that: EdgeLabel): Int = that match {
    case that: NoInfoEdgeLabel.type => 0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }

  override def propagateInfoList: Seq[PropagatableInfo] = Seq()
}
