package de.tu_darmstadt.veritas.VerificationInfrastructure

trait EdgeLabel

object NoInfoEdgeLabel extends EdgeLabel

/**
  * type of nodes in a proof graph, represents a single subgoal/step in a proof
  * @param spec the specification from which the goal should be proven
  * @param goal the goal to be proved
  * @tparam S type of the specification format
  * @tparam P type of the format for defining properties
  */
class ProofStep[S, P](val spec: S, val goal: P, val verificationStrategy: VerificationStrategy[S, P]) {
  protected val verificationStatus: VerificationStatus = NotStarted

  def getVerificationStatus(): VerificationStatus = verificationStatus

  /**
    * verify the single proof problem:
    * call the given verifier
    * node is only fully verified if all given assumptions are also fully verified
    * @param verifier
    * @param assumptions list of assumptions that shall be used for verification -> typically, all child nodes (connected with the same edge label)
    * @return updated ProofStep with new verification stati
    */
  def verify(verifier: Verifier[S, P],
             assumptions: Seq[(EdgeLabel, ProofStep[S, P])] = Seq()): ProofStep[S, P] = {
    val hypotheses = assumptions.map { a => (a._1, a._2.goal)}
    // TODO: Only look for Proved?
    // -> no, I would say we can be more generous and also recreate failed stati, if it works out with the edges
    // However, careful: if we get given MORE edges (= more assumptions) then before, we have to re-verify
    // since the assumptions could now contradict each other and then change the status
    // (IDEA: we may later add a special verifier that always does consistency checking before verifying...)
    // if we get fewer edges, that is ok as long as the edges that the individual configurations
    // used are still there.
    // In fact, the entire report has to be *filtered* to remove configurations that used edges that
    // are not in the given assumptions!
    verificationStatus match {
      case Outdated(f@Finished(report), pg) =>
        if (f.bestStatus.isVerified && assumptions.containsSlice(f.bestConf.usedEdges))
          return ProofStep(spec, goal, verificationStrategy, f)
      case _ => () // do nothing
    }
    val newStatus = verificationStrategy.callVerifier(verifier, spec, goal, assumptions)
    ProofStep(spec, goal, verificationStrategy, combineVerificationStatus(newStatus))
  }

  private def combineVerificationStatus(newStatus: VerificationStatus, pg: Option[ProofGraph[S, P]] = None): VerificationStatus = verificationStatus match {
    // TODO: what happens if verificationfailure is returned? we would loose the report of finished (verificationStatus)
      // good point. I tried to address this by allowing VerificationFailure to keep an old status (if that old status
      // was "Finished" - what do you think?
    case Finished(oldReport) => newStatus match {
      case Finished(newReport) => Finished(oldReport ++ newReport)
      case VerificationFailure(em, usv : Verifier[S, P], _, _) => VerificationFailure(Some(verificationStatus), pg, em, usv)
      case _ => newStatus
    }
    case _ => newStatus
  }

  // TODO: currently needed because VerificationStrategy does not know of the VerificationStatus
  // yes, that is ok!
  /**
    * computes if this proofstep is fully verified based on its children
    * @param edgeseq a sequence of edgelabels with the additional information if the connected children is fully verified
    * @return returns true when it is fully verified otherwise false
    */
  def fullyVerified(edgeseq: Seq[(EdgeLabel, Boolean)]): Boolean = {
    verificationStatus.isVerified && verificationStrategy.fullyVerified(edgeseq)
  }

  /**
    * mark a proof step as outdated, if there was a previous verification attempt
    * @param pg proof graph before the node became outdated
    * @return
    */
  def makeOutdated(pg: ProofGraph[S, P]): ProofStep[S, P] = verificationStatus match {
    case NotStarted => this //verifications that have not been started can never be Outdated
    case Outdated(prevs, pgold) => this //for the moment, avoid nesting multiple Outdated stati
    case _ => ProofStep(spec, goal, verificationStrategy, Outdated(verificationStatus, pg))
  }

  override def toString: String = s"ProofStep($verificationStatus, $verificationStrategy))"
}

object ProofStep {

  /*
  public constructor for a ProofStep
   */
  def apply[S, P](spec: S, goal: P, strategy: VerificationStrategy[S, P]): ProofStep[S, P] =
    new ProofStep(spec, goal, strategy)

  /*
  private constructor for a ProofStep which may manipulate the verification status
   */
  private def apply[S, P](spec: S, goal: P, verificationStrategy: VerificationStrategy[S, P], nverificationStatus: VerificationStatus): ProofStep[S, P] =
    new ProofStep[S, P](spec, goal, verificationStrategy) {
      override val verificationStatus = nverificationStatus
    }

  def unapply[S, P](arg: ProofStep[S, P]): Option[(S, P)] =
    Some((arg.spec, arg.goal))
}

