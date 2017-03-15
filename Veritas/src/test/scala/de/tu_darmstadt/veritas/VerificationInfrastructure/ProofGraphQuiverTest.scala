package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.Tactic
import org.scalatest.FunSuite
import quiver.{LEdge, LNode}

import scala.collection.GenSeq
import scala.util.Random

///**
//  * Created by andiderp on 20/01/2017.
//  */
//class ProofGraphQuiverTest extends FunSuite {
//  val topNode = LNode("Top", ProofStep("Spec", "Goal", Solve[String, String]()))
//  val child1 = LNode("Child1", ProofStep("Spec", "Goal", Solve[String, String]()))
//  val child2 = LNode("Child2", ProofStep("Spec", "Goal", Solve[String, String]()))
//  val edge1: VerificationEdge = LEdge("Top", "Child1", NoInfoProofEdgeLabel)
//  val edge2: VerificationEdge = LEdge("Top", "Child2", NoInfoProofEdgeLabel)
//  val provedVerifier = MockVerifier(MockProver())
//  val testGraph = ProofGraphQuiver(
//    Seq(topNode, child1, child2),
//    Seq(edge1, edge2))
//  .verifySingle(provedVerifier, "Child1")
//  .verifySingle(provedVerifier, "Child2")
//  .verifySingle(provedVerifier, "Top")
//
//  test("Graph should contain a new node and the defined edges after adding a node") {
//    val node = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
//    val newGraph = testGraph.addNode(node, Seq(LEdge("Top", "New", NoInfoProofEdgeLabel)))
//    val step = newGraph.find("New")
//    assert(step.nonEmpty)
//  }
//
//  test("Parents of added node should be set to outdated") {
//    val node = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
//    val newGraph = testGraph.addNode(node, Seq(LEdge("Top", "New", NoInfoProofEdgeLabel)))
//    val step = newGraph.find("Top")
//    assert(!newGraph.computeFullyVerified("Top"))
//    assert(step.get.getVerificationStatus().isInstanceOf[Outdated[String, String]])
//  }
//
//  test("Parents of added node should be set to outdated 2") {
//    val node = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
//    val newGraph = testGraph.addNode(node, Seq(LEdge("Child1", "New", NoInfoProofEdgeLabel)))
//    val top = newGraph.find("Top")
//    val child1 = newGraph.find("Child1")
//    assert(!newGraph.computeFullyVerified("Top"))
//    assert(!newGraph.computeFullyVerified("Child1"))
//    assert(top.get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
//    assert(child1.get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
//  }
//
//  test("Graph should be missing a node after removing a node") {
//    val newGraph = testGraph.removeNode(child1)
//    val step = newGraph.find("Child1")
//    assert(step.isEmpty)
//  }
//
//  test("Parents of removed node should be set to outdated") {
//    val newGraph = testGraph.removeNode(child1)
//    val step = newGraph.find("Top")
//    assert(!newGraph.computeFullyVerified("Top"))
//    assert(step.get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
//  }
//
//  test("Verifying single node does set parent to outdated") {
//    val node = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
//    val newGraph = testGraph.addNode(node, Seq(LEdge("Top", "New", NoInfoProofEdgeLabel)))
//        .verifySingle(provedVerifier, "New")
//    assert(newGraph.find("Top").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
//    assert(!newGraph.computeFullyVerified("Top"))
//    assert(!newGraph.find("Child1").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
//    assert(!newGraph.find("Child2").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
//    assert(!newGraph.find("New").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
//  }
//
//  test("Verifying single node does set transitive hull of parents to outdated") {
//    val node = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
//    val newGraph = testGraph.addNode(node, Seq(LEdge("Child1", "New", NoInfoProofEdgeLabel)))
//      .verifySingle(provedVerifier, "New")
//    assert(!newGraph.computeFullyVerified("Top"))
//    assert(newGraph.find("Top").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
//    assert(!newGraph.computeFullyVerified("Child1"))
//    assert(newGraph.find("Child1").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
//  }
//
//  test("Both parents are set to be outdated") {
//    val node1 = LNode("Parent", ProofStep("Spec", "Goal", Solve[String, String]()))
//    val node2 = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
//    val node3 = LNode("Child3", ProofStep("Spec", "Goal", Solve[String, String]()))
//
//    val newGraph = testGraph
//      .addNode(node1, Seq(LEdge("Child1", "Parent", NoInfoProofEdgeLabel)))
//      .addNode(node3, Seq(LEdge("Top", "Child3", NoInfoProofEdgeLabel)))
//      .verifySingle(provedVerifier, "Parent")
//      .verifySingle(provedVerifier, "Child3")
//      .addNode(node2, Seq(LEdge("Child2", "New", NoInfoProofEdgeLabel), LEdge("Parent", "New", NoInfoProofEdgeLabel)))
//
//    assert(newGraph.find("Parent").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
//    assert(newGraph.find("Child1").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
//    assert(newGraph.find("Child2").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
//    assert(!newGraph.computeFullyVerified("Parent"))
//    assert(!newGraph.computeFullyVerified("Child1"))
//    assert(!newGraph.computeFullyVerified("Child2"))
//    assert(newGraph.find("Child3").get.getVerificationStatus.isInstanceOf[Finished[String, String, String]])
//    assert(newGraph.computeFullyVerified("Child3"))
//  }
//
//  test("Verify node with children") {
//    val node = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
//    val switchStatusVerifier = MockVerifier(SwitchStatusProver())
//    val newGraph = testGraph
//      .verifySingle(provedVerifier, "Child1")
//      .verifySingle(provedVerifier, "Child2")
//      .addNode(node, Seq(LEdge("Top", "New", NoInfoProofEdgeLabel)))
//      .verifySingle(switchStatusVerifier, "Top")
//
//    assert(newGraph.find("Top").get.getVerificationStatus.isVerified)
//  }
//
//  test("Verify all nodes") {
//    val node1 = LNode("Parent", ProofStep("Spec", "Goal", Solve[String, String]()))
//    val node2 = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
//    val node3 = LNode("Child3", ProofStep("Spec", "Goal", Solve[String, String]()))
//
//    val newGraph = testGraph
//      .addNode(node1, Seq(LEdge("Child1", "Parent", NoInfoProofEdgeLabel)))
//      .addNode(node3, Seq(LEdge("Top", "Child3", NoInfoProofEdgeLabel)))
//      .addNode(node2, Seq(LEdge("Child2", "New", NoInfoProofEdgeLabel), LEdge("Parent", "New", NoInfoProofEdgeLabel)))
//      .verifyAll(provedVerifier)
//
//    assert(newGraph.find("Parent").get.getVerificationStatus.isVerified)
//    assert(newGraph.find("New").get.getVerificationStatus.isVerified)
//    assert(newGraph.find("Child1").get.getVerificationStatus.isVerified)
//    assert(newGraph.find("Child2").get.getVerificationStatus.isVerified)
//    assert(newGraph.find("Child3").get.getVerificationStatus.isVerified)
//    assert(newGraph.computeFullyVerified("Parent"))
//    assert(newGraph.computeFullyVerified("New"))
//    assert(newGraph.computeFullyVerified("Child1"))
//    assert(newGraph.computeFullyVerified("Child2"))
//    assert(newGraph.computeFullyVerified("Child3"))
//  }
//
//  test("Verify all nodes parallel") {
//    val node1 = LNode("Parent", ProofStep("Spec", "Goal", Solve[String, String]()))
//    val node2 = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
//    val node3 = LNode("Child3", ProofStep("Spec", "Goal", Solve[String, String]()))
//
//    val waitingVerifier = MockVerifier(WaitingProver())
//    val newGraph = testGraph
//      .addNode(node1, Seq(LEdge("Child1", "Parent", NoInfoProofEdgeLabel)))
//      .addNode(node3, Seq(LEdge("Top", "Child3", NoInfoProofEdgeLabel)))
//      .addNode(node2, Seq(LEdge("Child2", "New", NoInfoProofEdgeLabel), LEdge("Parent", "New", NoInfoProofEdgeLabel)))
//      .verifyAllPar(waitingVerifier)
//
//    assert(newGraph.find("Parent").get.getVerificationStatus.isVerified)
//    assert(newGraph.find("New").get.getVerificationStatus.isVerified)
//    assert(newGraph.find("Child1").get.getVerificationStatus.isVerified)
//    assert(newGraph.find("Child2").get.getVerificationStatus.isVerified)
//    assert(newGraph.find("Child3").get.getVerificationStatus.isVerified)
//    assert(newGraph.computeFullyVerified("Parent"))
//    assert(newGraph.computeFullyVerified("New"))
//    assert(newGraph.computeFullyVerified("Child1"))
//    assert(newGraph.computeFullyVerified("Child2"))
//    assert(newGraph.computeFullyVerified("Child3"))
//  }
//}

//case class MockVerifier(prover: Prover[String]) extends Verifier[String, String] {
//  override type V = String
//  override val desc: String = "MockVerifier"
//  override val transformer: GenSeq[Transformer[String, String, this.V]] = Seq()
//  override val provers: GenSeq[Prover[this.V]] = Seq()
//  override val supportedStrategies: Seq[VerificationStrategy[String, String]] = Seq(Solve())
//
//  /**
//    * combine calling all transformers with all provers and compose results into a single VerificationStatus
//    *
//    * @param spec       specification axioms/definitions
//    * @param hypotheses verified lemmas/assumptions
//    * @param goal       property/step to be proved
//    * @param strat      overall abstract strategy to be used for the current step
//    * @return Verification summary
//    */
//  override def verify(spec: String, hypotheses: Seq[String], goal: String, strat: VerificationStrategy[String, String]): VerifierStatus[String, String] =
//    // TODO: usedEdges cannot be passed because we only have the used goals
//    Finished(prover.callProver(""), this)
//}


case class MockProver() extends Prover[String] {
  override def supportedStrategies[S, P](): Seq[Tactic[S, P]] = ???

  override def callProver(problem: String): ProverStatus = Proved("")

}

case class SwitchStatusProver() extends Prover[String] {
  var count = -1
  override def supportedStrategies[S, P](): Seq[Tactic[S, P]] = ???

  override def callProver(problem: String): ProverStatus = {
    count = count + 1
    if (count == 0)
      Proved("")
    else
      Inconclusive("")
  }
}

case class ContradictingStatusProver() extends Prover[String] {
  var count = -1
  override def supportedStrategies[S, P](): Seq[Tactic[S, P]] = ???

  override def callProver(problem: String): ProverStatus = {
    count = count + 1
    if (count == 0)
      Proved("")
    else
      Disproved("")
  }

}

case class WaitingProver() extends Prover[String] {
  override def supportedStrategies[S, P](): Seq[Tactic[S, P]] = ???

  override def callProver(problem: String): ProverStatus = {
    val rnd = new Random()
    val waiting = rnd.nextInt(3000)
    Thread.sleep(waiting)
    Proved("")
  }
}

case class MockTransformer() extends Transformer[String, String, String] {
  override def transformProblem(spec: String, goal: String): String = ""
}
