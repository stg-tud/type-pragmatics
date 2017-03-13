package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import org.scalatest.FunSuite
import quiver.{LEdge, LNode}

/**
  * Created by andiderp on 20/01/2017.
  */
class ProofGraphXodusTest extends FunSuite {
  val topNode = LNode("Top", ProofStep("Spec", "Goal", Solve[String, String]()))
  val child1 = LNode("Child1", ProofStep("Spec", "Goal", Solve[String, String]()))
  val child2 = LNode("Child2", ProofStep("Spec", "Goal", Solve[String, String]()))
  val edge1: VerificationEdge = LEdge("Top", "Child1", NoInfoProofEdgeLabel)
  val edge2: VerificationEdge = LEdge("Top", "Child2", NoInfoProofEdgeLabel)
  val provedVerifier = MockVerifier(MockProver())
  def testGraph = {
    val file = File.createTempFile("veritas-xodus-test-store", "")
    file.delete()
    file.mkdir()
    println(s"Test entity store: $file")

    val g = new ProofGraphXodus[String, String](file)

    Seq(topNode, child1, child2).foreach(g.addProofStep(_))
    Seq(edge1, edge2).foreach(g.addProofEdge(_))
    g.verifySingle(provedVerifier, "Child1")
    g.verifySingle(provedVerifier, "Child2")
    g.verifySingle(provedVerifier, "Top")
    g
  }

  test("Graph should contain a new node and the defined edges after adding a node") {
    val node = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
    val newGraph = testGraph.addProofStep(node, Seq(LEdge("Top", "New", NoInfoProofEdgeLabel)))
    val step = newGraph.find("New")
    assert(step.nonEmpty)
  }

  test("Parents of added node should be set to outdated") {
    val node = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
    val newGraph = testGraph.addProofStep(node, Seq(LEdge("Top", "New", NoInfoProofEdgeLabel)))
    val step = newGraph.find("Top")
    assert(!newGraph.computeFullyVerified("Top"))
    assert(step.get.getVerificationStatus().isInstanceOf[Outdated[String, String]])
  }

  test("Parents of added node should be set to outdated 2") {
    val node = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
    val newGraph = testGraph.addProofStep(node, Seq(LEdge("Child1", "New", NoInfoProofEdgeLabel)))
    val top = newGraph.find("Top")
    val child1 = newGraph.find("Child1")
    assert(!newGraph.computeFullyVerified("Top"))
    assert(!newGraph.computeFullyVerified("Child1"))
    assert(top.get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
    assert(child1.get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  }

  test("Graph should be missing a node after removing a node") {
    val newGraph = testGraph.removeNode(child1)
    val step = newGraph.find("Child1")
    assert(step.isEmpty)
  }

  test("Parents of removed node should be set to outdated") {
    val newGraph = testGraph.removeNode(child1)
    val step = newGraph.find("Top")
    assert(!newGraph.computeFullyVerified("Top"))
    assert(step.get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  }

  test("Verifying single node does set parent to outdated") {
    val node = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
    val newGraph = testGraph.addProofStep(node, Seq(LEdge("Top", "New", NoInfoProofEdgeLabel)))
        .verifySingle(provedVerifier, "New")
    assert(newGraph.find("Top").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
    assert(!newGraph.computeFullyVerified("Top"))
    assert(!newGraph.find("Child1").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
    assert(!newGraph.find("Child2").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
    assert(!newGraph.find("New").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  }

  test("Verifying single node does set transitive hull of parents to outdated") {
    val node = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
    val newGraph = testGraph.addProofStep(node, Seq(LEdge("Child1", "New", NoInfoProofEdgeLabel)))
      .verifySingle(provedVerifier, "New")
    assert(!newGraph.computeFullyVerified("Top"))
    assert(newGraph.find("Top").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
    assert(!newGraph.computeFullyVerified("Child1"))
    assert(newGraph.find("Child1").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  }

  test("Both parents are set to be outdated") {
    val node1 = LNode("Parent", ProofStep("Spec", "Goal", Solve[String, String]()))
    val node2 = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
    val node3 = LNode("Child3", ProofStep("Spec", "Goal", Solve[String, String]()))

    val newGraph = testGraph
      .addProofStep(node1, Seq(LEdge("Child1", "Parent", NoInfoProofEdgeLabel)))
      .addProofStep(node3, Seq(LEdge("Top", "Child3", NoInfoProofEdgeLabel)))
      .verifySingle(provedVerifier, "Parent")
      .verifySingle(provedVerifier, "Child3")
      .addProofStep(node2, Seq(LEdge("Child2", "New", NoInfoProofEdgeLabel), LEdge("Parent", "New", NoInfoProofEdgeLabel)))

    assert(newGraph.find("Parent").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
    assert(newGraph.find("Child1").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
    assert(newGraph.find("Child2").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
    assert(!newGraph.computeFullyVerified("Parent"))
    assert(!newGraph.computeFullyVerified("Child1"))
    assert(!newGraph.computeFullyVerified("Child2"))
    assert(newGraph.find("Child3").get.getVerificationStatus.isInstanceOf[Finished[String, String, String]])
    assert(newGraph.computeFullyVerified("Child3"))
  }

  test("Verify node with children") {
    val node = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
    val switchStatusVerifier = MockVerifier(SwitchStatusProver())
    val newGraph = testGraph
      .verifySingle(provedVerifier, "Child1")
      .verifySingle(provedVerifier, "Child2")
      .addProofStep(node, Seq(LEdge("Top", "New", NoInfoProofEdgeLabel)))
      .verifySingle(switchStatusVerifier, "Top")

    assert(newGraph.find("Top").get.getVerificationStatus.isVerified)
  }

  test("Verify all nodes") {
    val node1 = LNode("Parent", ProofStep("Spec", "Goal", Solve[String, String]()))
    val node2 = LNode("New", ProofStep("Spec", "Goal", Solve[String, String]()))
    val node3 = LNode("Child3", ProofStep("Spec", "Goal", Solve[String, String]()))

    val newGraph = testGraph
      .addProofStep(node1, Seq(LEdge("Child1", "Parent", NoInfoProofEdgeLabel)))
      .addProofStep(node3, Seq(LEdge("Top", "Child3", NoInfoProofEdgeLabel)))
      .addProofStep(node2, Seq(LEdge("Child2", "New", NoInfoProofEdgeLabel), LEdge("Parent", "New", NoInfoProofEdgeLabel)))
      .verifyAll(provedVerifier)

    assert(newGraph.find("Parent").get.getVerificationStatus.isVerified)
    assert(newGraph.find("New").get.getVerificationStatus.isVerified)
    assert(newGraph.find("Child1").get.getVerificationStatus.isVerified)
    assert(newGraph.find("Child2").get.getVerificationStatus.isVerified)
    assert(newGraph.find("Child3").get.getVerificationStatus.isVerified)
    assert(newGraph.computeFullyVerified("Parent"))
    assert(newGraph.computeFullyVerified("New"))
    assert(newGraph.computeFullyVerified("Child1"))
    assert(newGraph.computeFullyVerified("Child2"))
    assert(newGraph.computeFullyVerified("Child3"))
  }

//  test("Verify all nodes parallel") {
//    val node1 = LNode("Parent", ProofStep("Spec", "Goal", Solve()))
//    val node2 = LNode("New", ProofStep("Spec", "Goal", Solve()))
//    val node3 = LNode("Child3", ProofStep("Spec", "Goal", Solve()))
//
//    val waitingVerifier = MockVerifier(WaitingProver())
//    val newGraph = testGraph
//      .addNode(node1, Seq(LEdge("Child1", "Parent", NoInfoProofEdgeLabel)))
//      .addNode(node3, Seq(LEdge("Top", "Child3", NoInfoProofEdgeLabel)))
//      .addNode(node2, Seq(LEdge("Child2", "New", NoInfoProofEdgeLabel), LEdge("Parent", "New", NoInfoProofEdgeLabel)))
//      .verifyAllPar(waitingVerifier)
//
//    assert(newGraph.get("Parent").get.getVerificationStatus.isVerified)
//    assert(newGraph.get("New").get.getVerificationStatus.isVerified)
//    assert(newGraph.get("Child1").get.getVerificationStatus.isVerified)
//    assert(newGraph.get("Child2").get.getVerificationStatus.isVerified)
//    assert(newGraph.get("Child3").get.getVerificationStatus.isVerified)
//    assert(newGraph.computeFullyVerified("Parent"))
//    assert(newGraph.computeFullyVerified("New"))
//    assert(newGraph.computeFullyVerified("Child1"))
//    assert(newGraph.computeFullyVerified("Child2"))
//    assert(newGraph.computeFullyVerified("Child3"))
//  }
}

