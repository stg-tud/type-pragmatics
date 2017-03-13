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
// TODO Why is this not a case class?
class ProofStep[S, P](val spec: S, val goal: P, val verificationStrategy: VerificationStrategy[S, P]) {
  // TODO Why is this not a regular constructor-argument field?
  protected val verificationStatus: VerifierStatus = Unknown

  def getVerificationStatus(): VerifierStatus = verificationStatus

  /**
    * verify the single proof problem:
    * call the given verifier
    * node is only fully verified if all given assumptions are also fully verified
    * @param verifier
    * @param assumptions list of assumptions that shall be used for verification -> typically, all child nodes (connected with the same edge label)
    * @return updated ProofStep with new verification stati
    */
  def verify(verifier: Verifier[S, P],
             assumptions: Seq[(ProofEdgeLabel, ProofStep[S, P])] = Seq()): ProofStep[S, P] = {
    def exitsEqualConfig(report: Map[VerificationConfiguration[Any, Any, Any], ProverStatus]): Boolean =
      report.keys.filter { case VerificationConfiguration(_, _, _, usedEdges, _) => usedEdges == assumptions }.nonEmpty

    // otherwise re-verify
    val newStatus = verificationStrategy.callVerifier(verifier, spec, goal, assumptions)
    ProofStep(spec, goal, verificationStrategy, combineVerificationStatus(newStatus))
  }

  private def filterFinishedReport(status: VerifierStatus, assumptions: Seq[(ProofEdgeLabel, ProofStep[S, P])]): VerifierStatus = status match {
    case Finished(report) => {
      val filteredReport = report.filter { case (vc, _) =>
        vc.usedEdges == assumptions
      }
      Finished(filteredReport)
    }
    case _ => status
  }

  private def combineVerificationStatus(newStatus: VerifierStatus, pg: Option[ProofGraphQuiver[S, P]] = None): VerifierStatus = verificationStatus match {
      // TODO: maybe consider further processing if verificationStatus is VerificationFailure
    case Finished(oldReport) => newStatus match {
      case Finished(newReport) => Finished(oldReport ++ newReport)
      case Failure(em, usv : Verifier[S, P], _, _) => Failure(em, usv, Some(verificationStatus), pg)
      case _ => newStatus
    }
    case _ => newStatus
  }

  /**
    * computes if this proofstep is fully verified based on its children
    * @param edgeseq a sequence of edgelabels with the additional information if the connected children is fully verified
    * @return returns true when it is fully verified otherwise false
    */
  def fullyVerified(edgeseq: Seq[(ProofEdgeLabel, Boolean)]): Boolean = {
    verificationStatus.isVerified && verificationStrategy.fullyVerified(edgeseq)
  }

  override def toString: String = s"ProofStep($verificationStatus, $verificationStrategy))"
}

object ProofStep {

  /*
  public constructor for a ProofStep
   */
  def apply[S, P](spec: S, goal: P, strategy: VerificationStrategy[S, P] = Solve[S, P]): ProofStep[S, P] =
    new ProofStep(spec, goal, strategy)

  /*
  private constructor for a ProofStep which may manipulate the verification status
   */
  private[VerificationInfrastructure] def apply[S, P](spec: S, goal: P, verificationStrategy: VerificationStrategy[S, P], nverificationStatus: VerifierStatus): ProofStep[S, P] =
    new ProofStep[S, P](spec, goal, verificationStrategy) {
      override val verificationStatus = nverificationStatus
    }

  def unapply[S, P](arg: ProofStep[S, P]): Option[(S, P)] =
    Some((arg.spec, arg.goal))
}

