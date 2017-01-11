package de.tu_darmstadt.veritas.VerificationInfrastructure

import scala.collection.GenSeq

/**
  * Structure for representing proof trees with verified and unverified parts
  * //TODO: add prover status to proof trees
  */
sealed abstract class ProofTree[S, P](val name: String,
                               val spec: S,
                               val goal: P,
                               val verificationStatus: VerificationStatus = NotStarted) {

  /**
    * use the given verifiers to attempt to verify a single node or leaf (no propagation to children!);
    * specific proof trees may have different ways of calling the verifies
    * @param verifier sequence of verifiers (e.g. ATPs)
    * @param strategy overall abstract VerificationStrategy for attempting verification of the current tree
    * @return VerificationStatus: status of verification of the proof tree
    */
  def verifySingle(verifier: GenSeq[Verifier[S, P]], strategy: VerificationStrategy = Solve): ProofTree[S, P]

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


}

case class ProofLeaf[S, P](override val name: String,
                           override val spec: S,
                           override val goal: P,
                           override val verificationStatus: VerificationStatus = NotStarted)
  extends ProofTree[S, P](name, spec, goal, verificationStatus) {

  def verifySingle(verifier: GenSeq[Verifier[S, P]], strategy: VerificationStrategy = Solve): ProofLeaf[S, P] =
    ???

  //    verifier exists { v => (v.supportedStrategies contains strategy) &&
//      v.verify(spec, Seq(), goal, strategy) }

  def addChildren(children: Seq[ProofTree[S, P]], newedge: VerificationStrategy = Solve): ProofTree[S, P] =
    ProofNode(name, spec, goal, newedge, children)

  def removeChildren(names: String*): ProofTree[S, P] = this
  def removeChildren(names: String*): ProofTree[S, P] = this //since a leaf has no children, nothing can be removed
}

case class ProofNode[S, P](override val name: String, override val spec: S, override val goal: P,
                           edge: VerificationStrategy, subgoals: Seq[ProofTree[S, P]])
  extends ProofTree[S, P](name, spec, goal) {
  def verifySingle(verifier: GenSeq[Verifier[S, P]], strategy: VerificationStrategy): ProofNode[S, P] =
    ???
  //    (subgoals forall { (pt: ProofTree[S, P]) => pt.verifySingle(verifier) }) &&
//      (verifier exists { (v: Verifier[S, P]) => (v.supportedStrategies contains strategy) &&
//        v.verify(spec, subgoals map { (pt: ProofTree[S, P]) => pt.goal }, goal, strategy)
//      })

  //default: don't change the strategy edge when adding children
  def addChildren(children: Seq[ProofTree[S, P]], newedge: VerificationStrategy = edge): ProofTree[S, P] =
  ProofNode(name, spec, goal, newedge, subgoals ++ children)

  def removeChildren(names: String*): ProofTree[S, P] =
    ProofNode(name, spec, goal, edge,
      subgoals filter { (pt: ProofTree[S, P]) => !(names contains pt.name) })
}
