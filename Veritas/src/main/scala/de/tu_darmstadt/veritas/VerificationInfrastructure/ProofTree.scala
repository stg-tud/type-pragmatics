package de.tu_darmstadt.veritas.VerificationInfrastructure

import scala.collection.GenSeq

/**
  * status of a particular verification attempt (for a node/leaf in a proof tree)
  *
  */
sealed trait VerificationStatus

case object NotStarted extends VerificationStatus

//TODO remember previous proof tree that created the previous verification status?
case class Outdated[S, P](prevs: ProverStatus, usedVerifier: Verifier[S, P]) extends VerificationStatus

case class Finished[S, P](ps: ProverStatus, usedVerifier: Verifier[S, P]) extends VerificationStatus

//TODO: have this status or not?
case class VerificationFailure[S, P](usedVerifier: Verifier[S, P]) extends VerificationStatus

/**
  * Structure for representing proof trees with verified and unverified parts
  * //TODO serialization of proof trees?
  */
sealed abstract class ProofTree[S, P](val name: String,
                                      val spec: S,
                                      val goal: P,
                                      val edge: VerificationStrategy = Solve) {

  //TODO: update methods also for spec / goal?
  /**
    * status of verification - publicly readable, but only verification methods should ever be able to set it!
    */
  val verificationStatus: VerificationStatus

  def nodeVerified: Boolean

  //TODO: how to register an updated child with the parents?? (google: update child of immutable tree)
  //TODO: maybe have a reset/clean for verify
  //TODO: skip verification of Finished (not outdated) nodes

  /**
    * use the given verifiers to attempt to verify a single node or leaf (no propagation to children!);
    * specific proof trees may have different ways of calling the verifies
    *
    * @param verifier (e.g. translating to TPTP and calling ATPs as provers)
    * @return VerificationStatus: status of verification of the proof tree
    */
  def verifySingleStep(verifier: Verifier[S, P]): ProofTree[S, P]

  /**
    * recursive verification of entire tree
    *
    * @param verifier (e.g. translating to TPTP and calling ATPs as provers)
    * @return VerificationStatus: status of verification of the proof tree
    */
  def verifyTree(verifier: Verifier[S, P]): ProofTree[S, P]

  /**
    * set a new verification edge
    * //TODO how to propagate this to parents??
    *
    * @param newedge
    * @return
    */
  def updateEdge(newedge: VerificationStrategy): ProofTree[S, P]

  /**
    * add children to a proof tree;
    * method can either be used by a strategy that automatically creates a proof tree,
    * or to manually manipulate a proof tree
    *
    * @param children
    * @return new proof tree with updated children and verification status
    */
  def addChildren(children: Seq[ProofTree[S, P]]): ProofTree[S, P]

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
      case Finished(ps, v) => Outdated(ps, v)
      case pvs => pvs
    }


}


class ProofLeaf[S, P](override val name: String,
                      override val spec: S,
                      override val goal: P,
                      override val edge: VerificationStrategy = Solve)
  extends ProofTree[S, P](name, spec, goal, edge) {

  override val verificationStatus: VerificationStatus = NotStarted

  override def nodeVerified: Boolean = verificationStatus match {
    case Finished(Proved(_,_), _) => true
    case _ => false
  }

  override def verifySingleStep(verifier: Verifier[S, P]): ProofLeaf[S, P] =
    ProofLeaf(name, spec, goal, edge, verifier.verify(spec, Seq(), goal, edge))

  override def verifyTree(verifier: Verifier[S, P]): ProofLeaf[S, P] =
    verifySingleStep(verifier)


  override def updateEdge(newedge: VerificationStrategy): ProofLeaf[S, P] =
    ProofLeaf(name, spec, goal, newedge)



  override def addChildren(children: Seq[ProofTree[S, P]]): ProofTree[S, P] =
    if (children.isEmpty)
      this
    else
      ProofNode(name, spec, goal, children, edge) //Verification status of new node will always be NotStarted //TODO: can we improve this?

  override def removeChildren(names: String*): ProofLeaf[S, P] = this //since a leaf has no children, nothing can be removed


  override def prettyPrint(): String = ???
}

object ProofLeaf {

  /*
  public constructor for a ProofLeaf
   */
  def apply[S, P](name: String, spec: S, goal: P, edge: VerificationStrategy = Solve): ProofLeaf[S, P] =
    ProofLeaf(name, spec, goal, edge, NotStarted)

  /*
  private constructor for a proof leaf which may manipulate the verification status
   */
  private def apply[S, P](n: String, s: S, g: P, e: VerificationStrategy, vs: VerificationStatus): ProofLeaf[S, P] =
    new ProofLeaf[S, P](n, s, g, e) {
      override val verificationStatus = vs
    }

  def unapply[S, P](arg: ProofLeaf[S, P]): Option[(String, S, P, VerificationStrategy)] =
    Some((arg.name, arg.spec, arg.goal, arg.edge))
}


class ProofNode[S, P](override val name: String,
                           override val spec: S,
                           override val goal: P,
                           val subgoals: GenSeq[ProofTree[S, P]],
                           override val edge: VerificationStrategy = Solve
                           )
  extends ProofTree[S, P](name, spec, goal, edge) {

  override val verificationStatus: VerificationStatus = NotStarted

  val nodeVerified: Boolean =
    (verificationStatus match {
      case Finished(Proved(_,_), _) => true
      case _ => false
    }) &&
      (subgoals forall { pt => pt.nodeVerified })

  override def verifySingleStep(verifier: Verifier[S, P]): ProofNode[S, P] =
    ProofNode(name, spec, goal, subgoals, edge,
      verifier.verify(spec, subgoals.map(s => s.goal).seq, goal, edge))

  override def verifyTree(verifier: Verifier[S, P]): ProofNode[S, P] = {
    val verifiedSubgoals = subgoals map { pt => pt.verifyTree(verifier) }
    val verifiedNode = verifySingleStep(verifier)
    verifiedNode match {
      case pn@ProofNode(n, s, g, sg, e) => ProofNode(n, s, g, verifiedSubgoals, e, pn.verificationStatus)
    }
  }

  override def updateEdge(newedge: VerificationStrategy): ProofNode[S, P] =
    ProofNode(name, spec, goal, subgoals, newedge)

  override def addChildren(children: Seq[ProofTree[S, P]]): ProofNode[S, P] =
    ProofNode(name, spec, goal, subgoals ++ children, edge, updatedVerificationStatus())

  override def removeChildren(names: String*): ProofTree[S, P] =
    ProofNode(name, spec, goal,
      subgoals filter { (pt: ProofTree[S, P]) => !(names contains pt.name) }, edge,
      updatedVerificationStatus())

  override def prettyPrint(): String = ???
}

object ProofNode {

  /*
  public constructor for a ProofNode
   */
  def apply[S, P](name: String, spec: S, goal: P, sg: GenSeq[ProofTree[S, P]], edge: VerificationStrategy): ProofNode[S, P] =
  ProofNode(name, spec, goal, sg, edge, NotStarted)

  /*
  private constructor for a proof node which may manipulate the verification status
   */
  private def apply[S, P](n: String, s: S, g: P, sg: GenSeq[ProofTree[S, P]], e: VerificationStrategy, vs: VerificationStatus): ProofNode[S, P] =
    new ProofNode[S, P](n, s, g, sg, e) {
      override val verificationStatus = vs
    }

  def unapply[S, P](arg: ProofNode[S, P]): Option[(String, S, P, GenSeq[ProofTree[S, P]], VerificationStrategy)] =
    Some((arg.name, arg.spec, arg.goal, arg.subgoals, arg.edge))
}

