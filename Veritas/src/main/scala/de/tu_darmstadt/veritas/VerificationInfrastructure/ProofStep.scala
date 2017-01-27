package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * type of nodes in a proof graph, represents a single subgoal/step in a proof
  * @param spec the specification from which the goal should be proven
  * @param goal the goal to be proved
  * @tparam S type of the specification format
  * @tparam P type of the format for defining properties
  */
//TODO: maybe put ProofStep class to a separate file later?
class ProofStep[S, P](val spec: S, val goal: P) {
  val verificationStatus: VerificationStatus = NotStarted
  val fullyVerified: Boolean = false

  /**
    * verify the single proof problem:
    * call the given verifier
    * node is only fully verified if all given assumptions are also fully verified
    * @param verifier
    * @param assumptions list of assumptions that shall be used for verification -> typically, all child nodes (connected with the same edge label)
    * @param strat strategy that shall be used for verification -> typically, from edge label
    * @return updated ProofStep with new verification stati
    */
  def verify(verifier: Verifier[S, P],
             assumptions: Seq[ProofStep[S, P]] = Seq(),
             strat: VerificationStrategy = Solve): ProofStep[S, P] = {
    val allAssumptions = assumptions map (pp => pp.goal)
    val newverificationStatus = verifier.verify(spec, allAssumptions, goal, strat)
    ProofStep(spec, goal, newverificationStatus).recomputefullyVerified(assumptions)
  }

  /**
    * function for recomputing fullyVerified in a proof step
    * this might be necessary for certain updates in the proof graph, to correctly propagate "fully verified"
    * @param assumptions
    * @return
    */
  //TODO is there a better solution for recomputing verification status than to manually pass the relevant assumptions...?
  def recomputefullyVerified(assumptions: Seq[ProofStep[S, P]] = Seq()): ProofStep[S, P] = {
    val allAssumptionsVerified = assumptions.forall(ps => ps.fullyVerified)
    val newfullyVerified = verificationStatus.isVerified && allAssumptionsVerified
    ProofStep(spec, goal, verificationStatus, newfullyVerified)
  }

  // TODO currently if a child of a child is making current node outdated we get a prev graph which is a nested outdated status, is this desirable?
  // -> Do you have an example? I would like to avoid nested Outdated stati.
  /**
    * mark a proof step as outdated, if there was a previous verification attempt
    * @param pg proof graph before the node became outdated
    * @return
    */
  def makeOutdated(pg: ProofGraph[S, P]): ProofStep[S, P] = {
    verificationStatus match {
      case NotStarted => this //verifications that have not been started can never be Outdated
      case Outdated(prevs, pgold) => this //for the moment, avoid nesting multiple Outdated stati
      case _ => ProofStep(spec, goal, Outdated(verificationStatus, pg))
    }
  }

  def makeFailed(message: String, usedVerifier: Verifier[S, P]): ProofStep[S, P] = {
    ProofStep(spec, goal, VerificationFailure(message, usedVerifier))
  }
}

object ProofStep {

  /*
  public constructor for a ProofStep
   */
  def apply[S, P](spec: S, goal: P): ProofStep[S, P] =
    new ProofStep(spec, goal)

  /*
  private constructor for a ProofStep which may manipulate the verification status
   */
  private def apply[S, P](spec: S, goal: P, nverificationStatus: VerificationStatus, nfullyVerified: Boolean = false): ProofStep[S, P] =
    new ProofStep[S, P](spec, goal) {
      override val verificationStatus = nverificationStatus
      override val fullyVerified: Boolean = nfullyVerified
    }

  def unapply[S, P](arg: ProofStep[S, P]): Option[(S, P)] =
    Some((arg.spec, arg.goal))
}

