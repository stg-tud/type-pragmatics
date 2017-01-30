package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * type of nodes in a proof graph, represents a single subgoal/step in a proof
  * @param spec the specification from which the goal should be proven
  * @param goal the goal to be proved
  * @tparam S type of the specification format
  * @tparam P type of the format for defining properties
  */
class ProofStep[S, P](val spec: S, val goal: P) {
  val verificationStatus: VerificationStatus = NotStarted
  val fullyVerified: Boolean = false
  // can only have on status per verificationstrategy
  // second element depicts how man children are fully verified
  // TODO: what happens if all children of a specifc strat are deleted
  // TODO: What happens if the children of one strategy want to make this node outdated (remove/add/update edge/node)
  val stratToStatus: Map[VerificationStrategy, (VerificationStatus, Int)] = Map()

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
    val numOfFullyVerifiedAssumptions = assumptions.count { ps => ps.fullyVerified }
    // TODO: best strat is currently only calculated (based on fullyverified children) when this method is called which is not desired
    val newStep = replaceVerificationStatusByStrategy(strat, newverificationStatus, numOfFullyVerifiedAssumptions, verifier)
    newStep.recomputefullyVerified(assumptions, strat)
  }

  private def replaceVerificationStatusByStrategy(strategy: VerificationStrategy, newStatus: VerificationStatus, numOfFullyVerifiedAssumptions: Int, verifier: Verifier[S, P]): ProofStep[S, P] = {
    val updatedStratToStatus = stratToStatus + (strategy -> (newStatus, numOfFullyVerifiedAssumptions))
    val stati = updatedStratToStatus.values.map { _._1 }.toSeq
    val contradictingStati = containsContradictingStati(stati)
    if (contradictingStati) {
      ProofStep(spec, goal, verificationStatus, nstratToStatus = updatedStratToStatus)
        .makeFailed("Verifier found a prove and a disprove for different strategies the same time.", verifier)
    } else {
      val bestStatus = calculateBestStatus(updatedStratToStatus)
      ProofStep(spec, goal, bestStatus, nstratToStatus = updatedStratToStatus)
    }
  }

  private def calculateBestStatus(stratToStatus: Map[VerificationStrategy, (VerificationStatus, Int)]): VerificationStatus = {
    val stati = stratToStatus.values.toSeq
    // sort first status (first element of tuple) ascending order and then
    // numOfFullyVerifiedChildren (second element of tuple) descending order
    val sortedByStatus = stati.sortBy { case (status, num) => (sortByVerificationStatus(status), -num) }
    sortedByStatus.head._1
  }

  private def containsContradictingStati(stati: Seq[VerificationStatus]): Boolean = {
    val proverStati = stati.foldLeft(Seq.empty[ProverStatus]) { case (l, status) =>
      status match {
        case Finished(ps, _, _) => Seq(ps) ++ l
        case _ => l
      }
    }
    val containsProved = proverStati.exists {
      case Proved(_) => true
      case _ => false
    }
    val containsDisproved = proverStati.exists {
      case Disproved(_) => true
      case _ => false
    }
    containsProved && containsDisproved
  }

  private def sortByVerificationStatus(status: VerificationStatus): Int = status match {
    // TODO think if we should consider outdated.finished.proved etc.
    case Finished(Proved(_), _, _) => 0
    case Finished(Disproved(_), _, _) => 1
    case Finished(Inconclusive, _, _) => 2
    case Outdated(_, _) => 3
    case VerificationFailure(_, _) => 4
    case NotStarted => 5
    case _ => 6
  }

  /**
    * function for recomputing fullyVerified in a proof step
    * this might be necessary for certain updates in the proof graph, to correctly propagate "fully verified"
    * @param assumptions
    * @return
    */
  //TODO is there a better solution for recomputing verification status than to manually pass the relevant assumptions...?
  def recomputefullyVerified(assumptions: Seq[ProofStep[S, P]] = Seq(), strategy: VerificationStrategy): ProofStep[S, P] = {
    val numOfFullyVerifiedChildren = assumptions.count(ps => ps.fullyVerified)
    val allAssumptionsVerified = assumptions.size == numOfFullyVerifiedChildren
    // TODO what happens if strategy is not registered?
    val latestStatusOfStrategy = stratToStatus(strategy)._1
    val updatedStratToStatus = stratToStatus + (strategy -> (latestStatusOfStrategy, numOfFullyVerifiedChildren))
    val newfullyVerified = verificationStatus.isVerified && allAssumptionsVerified
    ProofStep(spec, goal, verificationStatus, newfullyVerified, updatedStratToStatus)
  }

  // TODO currently if a child of a child is making current node outdated we get a prev graph which is a nested outdated status, is this desirable?
  // -> Do you have an example? I would like to avoid nested Outdated stati.
  /**
    * mark a proof step as outdated, if there was a previous verification attempt
    * @param pg proof graph before the node became outdated
    * @return
    */
  def makeOutdated(pg: ProofGraph[S, P], strategy: VerificationStrategy): ProofStep[S, P] = {
    // add a status for strategy if map does not contain the strategy
    val updatedStratToStatus = scala.collection.mutable.Map.empty ++ stratToStatus
    if (!stratToStatus.contains(strategy))
      updatedStratToStatus(strategy) = (NotStarted, 0)

    val (status, numOfFullyVerifiedChildren) = updatedStratToStatus(strategy)
    val newStatus = status match {
      case NotStarted => status //verifications that have not been started can never be Outdated
      case Outdated(prevs, pgold) => status //for the moment, avoid nesting multiple Outdated stati
      case _ => Outdated(status, pg)
    }
    // TODO whats happens to numOfFullyVerifiedChildren when outdating?
    updatedStratToStatus(strategy) = (newStatus, numOfFullyVerifiedChildren)
    val updatedVerificationStatus = if (status == verificationStatus) newStatus else verificationStatus
    ProofStep(spec, goal, updatedVerificationStatus, nstratToStatus = Map.empty ++ updatedStratToStatus)

  }

  private def makeFailed(message: String, usedVerifier: Verifier[S, P]): ProofStep[S, P] = {
    ProofStep(spec, goal, VerificationFailure(message, usedVerifier), nstratToStatus = stratToStatus)
  }

  override def toString: String = s"ProofStep($verificationStatus, $fullyVerified, $stratToStatus))"
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
  private def apply[S, P](spec: S, goal: P, nverificationStatus: VerificationStatus, nfullyVerified: Boolean = false, nstratToStatus: Map[VerificationStrategy, (VerificationStatus, Int)] = Map()): ProofStep[S, P] =
    new ProofStep[S, P](spec, goal) {
      override val verificationStatus = nverificationStatus
      override val fullyVerified: Boolean = nfullyVerified
      override val stratToStatus: Map[VerificationStrategy, (VerificationStatus, Int)] = nstratToStatus
    }

  def unapply[S, P](arg: ProofStep[S, P]): Option[(S, P)] =
    Some((arg.spec, arg.goal))
}

