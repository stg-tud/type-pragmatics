package de.tu_darmstadt.veritas.VerificationInfrastructure

import scala.collection.GenSeq

import quiver._

/**
  * status of a particular verification attempt (for a node/leaf in a proof tree)
  *
  */
sealed trait VerificationStatus {
  val isVerified: Boolean = false
}

case object NotStarted extends VerificationStatus

case class Outdated[S, P](prevs: VerificationStatus, previousProofGraph: ProofGraph[S, P]) extends VerificationStatus

case class Finished[S, P](ps: ProverStatus, usedVerifier: Verifier[S, P]) extends VerificationStatus {
  override val isVerified: Boolean = ps.isVerified
}

//TODO: have this status or not?
case class VerificationFailure[S, P](usedVerifier: Verifier[S, P]) extends VerificationStatus

/**
  * type of nodes in a proof graph, represents a single subgoal/step in a proof
  * @param spec the specification from which the goal should be proven
  * @param goal the goal to be proved
  * @tparam S type of the specification format
  * @tparam P type of the format for defining properties
  */
//TODO: maybe put this class to a different file later?
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
  def verifyNode(verifier: Verifier[S, P],
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


class ProofGraph[S, P] {

  //idea: other parts of the program should not be able to freely manipulate the internal graph
  //(mostly, because this could yield inconsistent verification stati)
  protected val graph: Graph[String, ProofStep[S, P], VerificationStrategy] = mkGraph(Seq(), Seq())

  //all valid proof graphs have to be acyclic
  require(!graph.hasLoop)

  /**
    * add a new node to the proof graph
    * if necessary: Outdate the verification status of the node and of all parents up to the root!
    * @param node
    * @param edges
    * @return the new, updated proof graph
    */
  def addNode(node: ProofNode[S, P], edges: Seq[VerificationEdge]): ProofGraph[S, P] = ???

  /**
    * remove a node from the proof graph
    * if necessary: Outdate the verification status of the node and of all parents up to the root!
    * @param node
    * @return
    */
  def removeNode(node: ProofNode[S, P]): ProofGraph[S, P] = ???

  /**
    * change a particular verification edge
    * @param oldedge
    * @param newedge
    * @return
    */
  def updateEdge(oldedge: VerificationEdge, newedge: VerificationEdge): ProofGraph[S, P] = ???

  /**
    * using a given verifier, verify a given node (assume that every node name is unique for now...)
    * if the node is a leaf, just attempt to verify the node directly via the Solve strategy
    * otherwise, call verifier once for each group of edges that have the same verification strategy as label,
    * passing the corresponding children as assumptions/hypotheses to the verifier
    * @param verifier
    * @param nodename
    * @return updated proof graph, where verification status is correctly propagated along the entire graph
    */
  def verifySingle(verifier: Verifier[S, P], nodename: String): ProofGraph[S, P] = ???

  /**
    * try to verify the entire tree (maybe reuse verifySingle for this?)
    * @param verifier
    * @return updated proof graph, where verification status is correctly propagated along the entire graph
    */
  def verifyAll(verifier: Verifier[S, P]): ProofGraph[S, P] = ???

  //TODO add functions for "pretty printing" the graph: simple ones that construct a string,
  // maybe functions that pretty print single nodes, etc.
  // - if possible, a function that generates a graph via graphviz (maybe it is possible to specify different colors
  //for different verification stati...? etc.



}

object ProofGraph {

  /**
    * public constructor
    */
  def apply[S, P](nodelist: Seq[ProofNode[S, P]], edgelist: Seq[VerificationEdge]): ProofGraph[S, P] =
    new ProofGraph[S, P] {
      override val graph = safeMkGraph(nodelist, edgelist) //ignores dangling edges
    }

  /**
    * private constructor for directly generating a new ProofGraph instance without having to reconstruct the graph
    */
  private def apply[S, P](newgraph: Graph[String, ProofStep[S, P], VerificationStrategy]): ProofGraph[S, P] =
    new ProofGraph[S, P] {
      override val graph = newgraph
    }

}

