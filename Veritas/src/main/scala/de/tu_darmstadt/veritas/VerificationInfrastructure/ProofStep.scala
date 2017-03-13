package de.tu_darmstadt.veritas.VerificationInfrastructure

trait ProofEdgeLabel extends Ordered[ProofEdgeLabel]

object NoInfoProofEdgeLabel extends ProofEdgeLabel {
  override def compare(that: ProofEdgeLabel): Int = that match {
    case that: NoInfoProofEdgeLabel.type => 0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

/**
  *
  * @param casename name of the induction case (should correspond to goal name of case?)
  * @param ihs induction hypotheses
  * @tparam P type of the format for defining properties/goals
  */
case class StructInductCase[P <: Ordered[P]](casename: String, ihs: Seq[P]) extends ProofEdgeLabel {
  override def compare(that: ProofEdgeLabel): Int = that match {
    case that: StructInductCase[P] =>
      val compare1 = this.casename compare that.casename
      if (compare1 != 0) return compare1
      val compare2 = this.ihs.size compare that.ihs.size
      if (compare2 != 0) return compare2
      val compare3 = this.ihs.zip(that.ihs).foreach { ih =>
        val compared = ih._1 compare ih._2
        if (compared != 0)
          return compared
      }
      0
    case _ => this.getClass.getCanonicalName.compare(that.getClass.getCanonicalName)
  }
}

/**
  * type of nodes in a proof graph, represents a single subgoal/step in a proof
  * @param spec the specification from which the goal should be proven
  * @param goal the goal to be proved
  * @tparam S type of the specification format
  * @tparam P type of the format for defining properties
  */
case class ProofStep[S, P](spec: S, goal: P, verificationStrategy: VerificationStrategy[S, P])

//  /**
//    * verify the single proof problem:
//    * call the given verifier
//    * node is only fully verified if all given assumptions are also fully verified
//    * @param verifier
//    * @param assumptions list of assumptions that shall be used for verification -> typically, all child nodes (connected with the same edge label)
//    * @return updated ProofStep with new verification stati
//    */
//  def verify(verifier: Verifier[S, P],
//             assumptions: Seq[(ProofEdgeLabel, ProofStep[S, P])] = Seq()): ProofStep[S, P] = {
//    def exitsEqualConfig(report: Map[VerifierConfiguration[Any, Any, Any], ProverStatus]): Boolean =
//      report.keys.filter { case VerifierConfiguration(_, _, _, usedEdges, _) => usedEdges == assumptions }.nonEmpty
//
//    // otherwise re-verify
//    val newStatus = verificationStrategy.callVerifier(verifier, spec, goal, assumptions)
//    ProofStep(spec, goal, verificationStrategy, combineVerificationStatus(newStatus))
//  }
//
//  /**
//    * computes if this proofstep is fully verified based on its children
//    * @param edgeseq a sequence of edgelabels with the additional information if the connected children is fully verified
//    * @return returns true when it is fully verified otherwise false
//    */
//  def fullyVerified(edgeseq: Seq[(ProofEdgeLabel, Boolean)]): Boolean = {
//    verificationStatus.isVerified && verificationStrategy.fullyVerified(edgeseq)
//  }
