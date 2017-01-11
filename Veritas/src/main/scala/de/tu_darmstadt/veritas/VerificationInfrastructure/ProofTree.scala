package de.tu_darmstadt.veritas.VerificationInfrastructure

import scala.collection.GenSeq

/**
  * status of a particular verification attempt (for a node/leaf in a proof tree)
  *
  */
sealed trait VerificationStatus

case object NotStarted extends VerificationStatus

case class Outdated(prevs: ProverStatus) extends VerificationStatus

case class Finished(ps: ProverStatus) extends VerificationStatus

//TODO maybe include an error status?

/**
  * Structure for representing proof trees with verified and unverified parts
  * //TODO: should verification status be public, so that everybody can just create a verified node?
  */
sealed abstract class ProofTree[S, P](val name: String,
                                      val spec: S,
                                      val goal: P,
                                      val verificationStatus: VerificationStatus = NotStarted) {

  /**
    * use the given verifiers to attempt to verify a single node or leaf (no propagation to children!);
    * specific proof trees may have different ways of calling the verifies
    *
    * @param verifier (e.g. translating to TPTP and calling ATPs as provers)
    * @param strategy overall abstract VerificationStrategy for attempting verification of the current tree
    * @return VerificationStatus: status of verification of the proof tree
    */
  def verifySingleStep(verifier: Verifier[S, P], strategy: VerificationStrategy = Solve): ProofTree[S, P]

  /**
    * recursive verification of entire tree
    *
    * @param verifier (e.g. translating to TPTP and calling ATPs as provers)
    * @param strategy overall abstract VerificationStrategy for attempting verification of the current tree
    * @return VerificationStatus: status of verification of the proof tree
    */
  def verifyTree(verifier: Verifier[S, P], strategy: VerificationStrategy = Solve): ProofTree[S, P]

  /**
    * add children to a proof tree;
    * method can either be used by a strategy that automatically creates a proof tree,
    * or to manually manipulate a proof tree
    *
    * @param children
    * @param newedge general strategy that should be used to verify the parent node from the children
    * @return new proof tree with updated children and verification status
    */
  def addChildren(children: Seq[ProofTree[S, P]], newedge: VerificationStrategy = Solve): ProofTree[S, P]

  /**
    * remove children from a proof tree (if they exist), by name
    * method can either be used by a strategy that automatically creates a proof tree,
    * or to manually manipulate a proof tree
    *
    * @param names
    * @return new proof tree with updated children and verification status
    */
  def removeChildren(names: String*): ProofTree[S, P]

  /**
    * pretty print an entire proof tree recursively
    * //TODO maybe rather reuse prettyprint trait from backend/util?
    *
    * @return pretty-printed String representation of proof tree
    */
  def prettyPrint(): String

  /**
    * return an updated verification status
    * if there was a previous verification, mark status as outdated, otherwise copy status
    *
    * @return
    */
  protected def updatedVerificationStatus(): VerificationStatus =
    verificationStatus match {
      case Finished(ps) => Outdated(ps)
      case pvs => pvs
    }


}

case class ProofLeaf[S, P](override val name: String,
                           override val spec: S,
                           override val goal: P,
                           override val verificationStatus: VerificationStatus = NotStarted)
  extends ProofTree[S, P](name, spec, goal, verificationStatus) {

  override def verifySingleStep(verifier: Verifier[S, P], strategy: VerificationStrategy = Solve): ProofTree[S, P] =
    ProofLeaf(name, spec, goal, verifier.verify(spec, Seq(), goal, strategy))

  override def verifyTree(verifier: Verifier[S, P], strategy: VerificationStrategy): ProofTree[S, P] =
    verifySingleStep(verifier, strategy)


  override def addChildren(children: Seq[ProofTree[S, P]], newedge: VerificationStrategy = Solve): ProofTree[S, P] =
    ProofNode(name, spec, goal, newedge, children, updatedVerificationStatus())

  override def removeChildren(names: String*): ProofTree[S, P] = this //since a leaf has no children, nothing can be removed


  override def prettyPrint(): String = ???
}

case class ProofNode[S, P](override val name: String,
                           override val spec: S,
                           override val goal: P,
                           edge: VerificationStrategy,
                           subgoals: GenSeq[ProofTree[S, P]],
                           override val verificationStatus: VerificationStatus = NotStarted)
  extends ProofTree[S, P](name, spec, goal, verificationStatus) {

  val nodeVerified: Boolean = subgoals forall { pt =>
    pt match {
      case pn@ProofNode(_, _, _, _, _, _) => pn.nodeVerified
      case ProofLeaf(_, _, _, Finished(Proved)) => true
      case ProofLeaf(_, _, _, _) => false
    }
  }

  override def verifySingleStep(verifier: Verifier[S, P], strategy: VerificationStrategy): ProofTree[S, P] =
    ProofNode(name, spec, goal, strategy, subgoals,
      verifier.verify(spec, subgoals.map(s => s.goal).seq, goal, strategy))

  override def verifyTree(verifier: Verifier[S, P], strategy: VerificationStrategy): ProofTree[S, P] = {
    val verifiedSubgoals = subgoals map { pt => pt.verifyTree(verifier, strategy) }
    val verifiedNode = verifySingleStep(verifier, strategy)
    verifiedNode match {
      case ProofNode(n, s, g, e, sg, vs) => ProofNode(n, s, g, e, verifiedSubgoals, vs)
      case ProofLeaf(_, _, _, _) => sys.error("Error in verifyTree: verified Node suddenly became a leaf!") //This case should not happen!
    }
  }

  //default: don't change the strategy edge when adding children
  override def addChildren(children: Seq[ProofTree[S, P]], newedge: VerificationStrategy = edge): ProofTree[S, P] =
    ProofNode(name, spec, goal, newedge, subgoals ++ children, updatedVerificationStatus())

  override def removeChildren(names: String*): ProofTree[S, P] =
    ProofNode(name, spec, goal, edge,
      subgoals filter { (pt: ProofTree[S, P]) => !(names contains pt.name) },
      updatedVerificationStatus())

  override def prettyPrint(): String = ???
}
