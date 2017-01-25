package de.tu_darmstadt.veritas.VerificationInfrastructure

import org.scalatest.FunSuite
import quiver.{LEdge, LNode}

import scala.collection.GenSeq

/**
  * Created by andiderp on 20/01/2017.
  */
class ProofGraphTest extends FunSuite {
  val topNode = LNode("Top", ProofStep("Spec", "Goal"))
  val child1 = LNode("Child1", ProofStep("Spec", "Goal"))
  val child2 = LNode("Child2", ProofStep("Spec", "Goal"))
  val edge1: VerificationEdge = LEdge("Top", "Child1", Solve)
  val edge2: VerificationEdge = LEdge("Top", "Child2", Solve)
  val provedVerifier = MockVerifier(MockProver())
  val testGraph = ProofGraph(
    Seq(topNode, child1, child2),
    Seq(edge1, edge2))
  .verifySingle(provedVerifier, "Child1")
  .verifySingle(provedVerifier, "Child2")
  .verifySingle(provedVerifier, "Top")

  test("Graph should contain a new node and the defined edges after adding a node") {
    val node = LNode("New", ProofStep("Spec", "Goal"))
    val newGraph = testGraph.addNode(node, Seq(LEdge("Top", "New", Solve)))
    val step = newGraph.get("New")
    assert(step.nonEmpty)
  }

  test("Parents of added node should be set to outdated") {
    val node = LNode("New", ProofStep("Spec", "Goal"))
    val newGraph = testGraph.addNode(node, Seq(LEdge("Top", "New", Solve)))
    val step = newGraph.get("Top")
    assert(!step.get.fullyVerified)
    assert(step.get.verificationStatus.isInstanceOf[Outdated[String, String]])
  }

  test("Parents of added node should be set to outdated 2") {
    val node = LNode("New", ProofStep("Spec", "Goal"))
    val newGraph = testGraph.addNode(node, Seq(LEdge("Child1", "New", Solve)))
    val top = newGraph.get("Top")
    val child1 = newGraph.get("Child1")
    assert(!top.get.fullyVerified)
    assert(!child1.get.fullyVerified)
    assert(top.get.verificationStatus.isInstanceOf[Outdated[String, String]])
    assert(child1.get.verificationStatus.isInstanceOf[Outdated[String, String]])
  }

  test("Graph should be missing a node after removing a node") {
    val newGraph = testGraph.removeNode(child1)
    val step = newGraph.get("Child1")
    assert(step.isEmpty)
  }

  test("Parents of removed node should be set to outdated") {
    val newGraph = testGraph.removeNode(child1)
    val step = newGraph.get("Top")
    assert(!step.get.fullyVerified)
    assert(step.get.verificationStatus.isInstanceOf[Outdated[String, String]])
  }

  test("VerificationStrategy of edge changed") {
    val newEdge: VerificationEdge = LEdge("Top", "Child1", Induction)
    val newGraph = testGraph.updateEdge(edge1, newEdge)
  }

  test("Verifying single node does set parent to outdated") {
    val node = LNode("New", ProofStep("Spec", "Goal"))
    val newGraph = testGraph.addNode(node, Seq(LEdge("Top", "New", Solve)))
        .verifySingle(provedVerifier, "New")
    assert(newGraph.get("Top").get.verificationStatus.isInstanceOf[Outdated[String, String]])
    assert(!newGraph.get("Top").get.fullyVerified)
    assert(!newGraph.get("Child1").get.verificationStatus.isInstanceOf[Outdated[String, String]])
    assert(!newGraph.get("Child2").get.verificationStatus.isInstanceOf[Outdated[String, String]])
    assert(!newGraph.get("New").get.verificationStatus.isInstanceOf[Outdated[String, String]])
  }

  test("Verifying single node does set transitive hull of parents to outdated") {
    val node = LNode("New", ProofStep("Spec", "Goal"))
    val newGraph = testGraph.addNode(node, Seq(LEdge("Child1", "New", Solve)))
      .verifySingle(provedVerifier, "New")
    assert(!newGraph.get("Top").get.fullyVerified)
    assert(newGraph.get("Top").get.verificationStatus.isInstanceOf[Outdated[String, String]])
    assert(!newGraph.get("Child1").get.fullyVerified)
    assert(newGraph.get("Child1").get.verificationStatus.isInstanceOf[Outdated[String, String]])
  }

  test("Both parents are set to be outdated") {
    val node1 = LNode("Parent", ProofStep("Spec", "Goal"))
    val node2 = LNode("New", ProofStep("Spec", "Goal"))
    val node3 = LNode("Child3", ProofStep("Spec", "Goal"))

    val newGraph = testGraph
      .addNode(node1, Seq(LEdge("Child1", "Parent", Solve)))
      .addNode(node3, Seq(LEdge("Top", "Child3", Solve)))
      .verifySingle(provedVerifier, "Parent")
      .verifySingle(provedVerifier, "Child3")
      .addNode(node2, Seq(LEdge("Child2", "New", Solve), LEdge("Parent", "New", Solve)))

    assert(newGraph.get("Parent").get.verificationStatus.isInstanceOf[Outdated[String, String]])
    assert(newGraph.get("Child1").get.verificationStatus.isInstanceOf[Outdated[String, String]])
    assert(newGraph.get("Child2").get.verificationStatus.isInstanceOf[Outdated[String, String]])
    assert(!newGraph.get("Parent").get.fullyVerified)
    assert(!newGraph.get("Child1").get.fullyVerified)
    assert(!newGraph.get("Child2").get.fullyVerified)
    assert(newGraph.get("Child3").get.verificationStatus.isInstanceOf[Finished[String, String]])
    assert(newGraph.get("Child3").get.fullyVerified)
  }

  test("Verify node with children") {
    val node = LNode("New", ProofStep("Spec", "Goal"))
    val switchStatusVerifier = MockVerifier(SwitchStatusProver())
    val newGraph = testGraph
      .verifySingle(provedVerifier, "Child1")
      .verifySingle(provedVerifier, "Child2")
      .addNode(node, Seq(LEdge("Top", "New", Induction)))
      .verifySingle(switchStatusVerifier, "Top")

    assert(newGraph.get("Top").get.verificationStatus.isVerified)
  }

  test("Verify node with contradicting proverstati") {
    val node = LNode("New", ProofStep("Spec", "Goal"))
    val switchStatusVerifier = MockVerifier(ContradictingStatusProver())
    val newGraph = testGraph
      .verifySingle(provedVerifier, "Child1")
      .verifySingle(provedVerifier, "Child2")
      .addNode(node, Seq(LEdge("Top", "New", Induction)))
      .verifySingle(switchStatusVerifier, "Top")

    assert(newGraph.get("Top").get.verificationStatus.isInstanceOf[VerificationFailure[String, String]])
  }
}

case class MockVerifier(prover: Prover[String]) extends Verifier[String, String] {
  override type V = this.type
  override val transformer: GenSeq[Transformer[String, String, MockVerifier.this.type]] = Seq()
  override val provers: GenSeq[Prover[MockVerifier.this.type]] = Seq()
  override val supportedStrategies: Seq[VerificationStrategy] = Seq(Solve)

  /**
    * combine calling all transformers with all provers and compose results into a single VerificationStatus
    *
    * @param spec       specification axioms/definitions
    * @param hypotheses verified lemmas/assumptions
    * @param goal       property/step to be proved
    * @param strat      overall abstract strategy to be used for the current step
    * @return Verification summary
    */
  override def verify(spec: String, hypotheses: Seq[String], goal: String, strat: VerificationStrategy): VerificationStatus = Finished(prover.callProver(), this)
}


case class MockProver() extends Prover[String]("") {
  override val supportedStrategies: Seq[VerificationStrategy] = Seq(Solve)

  override def callProver(): ProverStatus = Proved(MockTransformer(), this)
}

case class SwitchStatusProver() extends Prover[String]("") {
  var count = -1
  override val supportedStrategies: Seq[VerificationStrategy] = Seq(Solve, Induction)

  override def callProver(): ProverStatus = {
    count = count + 1
    if (count == 0)
      Proved(MockTransformer(), this)
    else
      Inconclusive
  }
}

case class ContradictingStatusProver() extends Prover[String]("") {
  var count = -1
  override val supportedStrategies: Seq[VerificationStrategy] = Seq(Solve, Induction)

  override def callProver(): ProverStatus = {
    count = count + 1
    if (count == 0)
      Proved(MockTransformer(), this)
    else
      Disproved(MockTransformer(), this)
  }
}

case class MockTransformer() extends Transformer[String, String, MockVerifier]("", "") {
  override def transformProblem: MockVerifier = MockVerifier(MockProver())
}
