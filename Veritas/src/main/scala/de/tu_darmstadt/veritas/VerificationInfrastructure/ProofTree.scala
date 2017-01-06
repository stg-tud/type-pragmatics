package de.tu_darmstadt.veritas.VerificationInfrastructure

/**
  * Structure for representing proof trees with verified and unverified parts
  * //TODO: add prover status to proof trees
  */
abstract class ProofTree[S, P](val name: String, val spec: S, val goal: P) {

  def verify(verifier: Seq[Verifier[S, P]], strategy: VerificationStrategy = Solve): Boolean

  def addChildren(children: Seq[ProofTree[S, P]], newedge: VerificationStrategy = Solve): ProofTree[S, P]

  def removeChildren(names: String*): ProofTree[S, P]
}

case class ProofLeaf[S, P](override val name: String, override val spec: S, override val goal: P)
  extends ProofTree[S, P](name, spec, goal) {
  def verify(verifier: Seq[Verifier[S, P]], strategy: VerificationStrategy = Solve): Boolean =
    verifier exists { v => (v.supportedStrategies contains strategy) &&
      v.verify(spec, Seq(), goal, strategy) }

  def addChildren(children: Seq[ProofTree[S, P]], newedge: VerificationStrategy = Solve): ProofTree[S, P] =
    ProofNode(name, spec, goal, newedge, children)

  def removeChildren(names: String*): ProofTree[S, P] = this
}

case class ProofNode[S, P](override val name: String, override val spec: S, override val goal: P,
                           edge: VerificationStrategy, subgoals: Seq[ProofTree[S, P]])
  extends ProofTree[S, P](name, spec, goal) {
  def verify(verifier: Seq[Verifier[S, P]], strategy: VerificationStrategy): Boolean =
    (subgoals forall { (pt: ProofTree[S, P]) => pt.verify(verifier) }) &&
      (verifier exists { (v: Verifier[S, P]) => (v.supportedStrategies contains strategy) &&
        v.verify(spec, subgoals map { (pt: ProofTree[S, P]) => pt.goal }, goal, strategy)
      })

  //default: don't change the strategy edge when adding children
  def addChildren(children: Seq[ProofTree[S, P]], newedge: VerificationStrategy = edge): ProofTree[S, P] =
  ProofNode(name, spec, goal, newedge, subgoals ++ children)

  def removeChildren(names: String*): ProofTree[S, P] =
    ProofNode(name, spec, goal, edge,
      subgoals filter { (pt: ProofTree[S, P]) => !(names contains pt.name) })
}
