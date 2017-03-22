package de.tu_darmstadt.veritas.VerificationInfrastructure

import java.io.File

import org.scalacheck._
import Arbitrary.arbitrary
import org.scalacheck.util.Pretty
import org.scalatest._

import scala.tools.nsc.doc.base.comment.OrderedList
//import org.scalatest.prop.PropertyChecks
import org.scalacheck.Prop.forAll

//simple wrapper classes for simulating content of proof graphs
case class Spec[T <: Comparable[T]](spec: T) extends Comparable[Spec[T]] {
  override def compareTo(that: Spec[T]): Int = this.spec compareTo that.spec
}

case class Goal[T <: Comparable[T]](goal: T) extends Comparable[Goal[T]] {
  override def compareTo(that: Goal[T]): Int = this.goal compareTo that.goal
}

/**
  * Testing the proof graph Xodus implementation with basic types only
  */
class ProofGraphXodusTest extends FunSuite {

  //initializing a new test graph and registering all required basic property types
  def initializePGXodus(printStorePath: Boolean = false): ProofGraphXodus[Spec[String], Goal[String]] = {
    val file = File.createTempFile("test-store", "")
    file.delete()
    file.mkdir()
    if (printStorePath) println(s"Test entity store: $file")

    val g: ProofGraphXodus[Spec[String], Goal[String]] =
      new ProofGraphXodus[Spec[String], Goal[String]](file)

    PropertyTypes.registerPropertyType[Spec[String]](g.store)
    PropertyTypes.registerPropertyType[Goal[String]](g.store)

    g

  }

  def initializePGXodusSimpleString(printStorePath: Boolean = false): ProofGraphXodus[String, String] = {
    val file = File.createTempFile("test-store", "")
    file.delete()
    file.mkdir()
    if (printStorePath) println(s"Test entity store: $file")

    val g: ProofGraphXodus[String, String] =
      new ProofGraphXodus[String, String](file)

    g

  }

  //these generators will only work with basic types T!
  def genNames: Gen[String] = Gen.alphaNumStr

  def genSpecs: Gen[Spec[String]] = for {x <- arbitrary[String]} yield Spec[String](x)

  def genGoals: Gen[Goal[String]] = for {x <- arbitrary[String]} yield Goal[String](x)

  //convenience method for testing properties inside normal tests and assessing/pretty-printing the result
  def testProp(p: Prop) = {
    val testresult: Test.Result = Test.check(Test.Parameters.default, p)

    testresult.status match {
      case Test.Proved(_) | Test.Passed => println("Success after " + testresult.succeeded + " tests.")
      case Test.Failed(args, _) => {
        println("Failed after " + testresult.succeeded + " successful tests with: ")
        for (a <- args) println(a.prettyArg.apply(Pretty.defaultParams))
      }
      case s => println(s)
    }

    assert(testresult.passed)
  }

  test("Created obligations contain given specs and goals (String)") {
    def obligationCreationBasic = forAll(genNames, genSpecs, genGoals) { (name: String, s: Spec[String], g: Goal[String]) => {
      val graph: ProofGraphXodus[Spec[String], Goal[String]] = initializePGXodus()
      val obl = graph.newObligation(s, g)

      obl.spec == s && obl.goal == g
    }
    }

    testProp(obligationCreationBasic)
  }


  test("One simple String insertion/retrieval works") {
    val graph: ProofGraphXodus[String, String] = initializePGXodusSimpleString()
    val obl = graph.newObligation("S", "G")
    graph.storeObligation("test", obl)
    val retobl = graph.findObligation("test")

    assert(retobl.nonEmpty)
    assert(obl.spec == retobl.get.spec)
    assert(obl.goal == retobl.get.goal)

  }

  test("One simple Spec[String],Goal[String] insertion/retrieval works") {
    val graph: ProofGraphXodus[Spec[String], Goal[String]] = initializePGXodus()
    val obl = graph.newObligation(Spec("S"), Goal("G"))
    graph.storeObligation("test", obl)
    val retobl = graph.findObligation("test")

    assert(retobl.nonEmpty)
    assert(obl.spec == retobl.get.spec)
    assert(obl.goal == retobl.get.goal)

  }


  test("Inserted root nodes can be retrieved in general (only Strings!)") {
    def insertRetrievePropBasicString() =
      forAll(genNames, Arbitrary.arbitrary[String], Arbitrary.arbitrary[String]) { (name: String, s: String, g: String) => {
        val graph: ProofGraphXodus[String, String] = initializePGXodusSimpleString()
        val obl = graph.newObligation(s, g)
        graph.storeObligation(name, obl)
        val retobl = graph.findObligation(name)

        retobl.nonEmpty && (obl.spec == retobl.get.spec) && (obl.goal == retobl.get.goal)
      }
      }

    testProp(insertRetrievePropBasicString)
  }

  test("Inserted root nodes can be retrieved in general (only Spec[String]/Goal[String]!)") {
    def insertRetrievePropBasic() =
      forAll(genNames, genSpecs, genGoals) { (name: String, s: Spec[String], g: Goal[String]) => {
        val graph: ProofGraphXodus[Spec[String], Goal[String]] = initializePGXodus()
        val obl = graph.newObligation(s, g)
        graph.storeObligation(name, obl)
        val retobl = graph.findObligation(name)

        retobl.nonEmpty && (obl.spec == retobl.get.spec) && (obl.goal == retobl.get.goal)
      }
      }

    testProp(insertRetrievePropBasic)
  }





  //Instantiating
  //  val topNode = LNode("Top", Obligation("Spec", "Goal", Solve[String, String]()))
  //  val child1 = LNode("Child1", Obligation("Spec", "Goal", Solve[String, String]()))
  //  val child2 = LNode("Child2", Obligation("Spec", "Goal", Solve[String, String]()))
  //  val edge1: VerificationEdge = LEdge("Top", "Child1", NoInfoEdgeLabel)
  //  val edge2: VerificationEdge = LEdge("Top", "Child2", NoInfoEdgeLabel)
  //  val provedVerifier = MockVerifier(MockProver())
  //  def testGraph = {
  //    val file = File.createTempFile("veritas-xodus-test-store", "")
  //    file.delete()
  //    file.mkdir()
  //    println(s"Test entity store: $file")
  //
  //    val g = new ProofGraphXodus[String, String](file)
  //
  //    Seq(topNode, child1, child2).foreach(g.addProofStep(_))
  //    Seq(edge1, edge2).foreach(g.addProofEdge(_))
  //    g.verifySingle(provedVerifier, "Child1")
  //    g.verifySingle(provedVerifier, "Child2")
  //    g.verifySingle(provedVerifier, "Top")
  //    g
  //  }
  //
  //  test("Graph should contain a new node and the defined edges after adding a node") {
  //    val node = LNode("New", Obligation("Spec", "Goal", Solve[String, String]()))
  //    val newGraph = testGraph.addProofStep(node, Seq(LEdge("Top", "New", NoInfoEdgeLabel)))
  //    val step = newGraph.find("New")
  //    assert(step.nonEmpty)
  //  }
  //
  //  test("Parents of added node should be set to outdated") {
  //    val node = LNode("New", Obligation("Spec", "Goal", Solve[String, String]()))
  //    val newGraph = testGraph.addProofStep(node, Seq(LEdge("Top", "New", NoInfoEdgeLabel)))
  //    val step = newGraph.find("Top")
  //    assert(!newGraph.computeFullyVerified("Top"))
  //    //assert(step.get.getVerificationStatus().isInstanceOf[Outdated[String, String]])
  //  }
  //
  //  test("Parents of added node should be set to outdated 2") {
  //    val node = LNode("New", Obligation("Spec", "Goal", Solve[String, String]()))
  //    val newGraph = testGraph.addProofStep(node, Seq(LEdge("Child1", "New", NoInfoEdgeLabel)))
  //    val top = newGraph.find("Top")
  //    val child1 = newGraph.find("Child1")
  //    assert(!newGraph.computeFullyVerified("Top"))
  //    assert(!newGraph.computeFullyVerified("Child1"))
  //    //assert(top.get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  //    //assert(child1.get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
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
  //    //assert(step.get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  //  }
  //
  //  test("Verifying single node does set parent to outdated") {
  //    val node = LNode("New", Obligation("Spec", "Goal", Solve[String, String]()))
  //    val newGraph = testGraph.addProofStep(node, Seq(LEdge("Top", "New", NoInfoEdgeLabel)))
  //        .verifySingle(provedVerifier, "New")
  //    //assert(newGraph.find("Top").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  //    assert(!newGraph.computeFullyVerified("Top"))
  //    //assert(!newGraph.find("Child1").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  //    //assert(!newGraph.find("Child2").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  //    //assert(!newGraph.find("New").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  //  }
  //
  //  test("Verifying single node does set transitive hull of parents to outdated") {
  //    val node = LNode("New", Obligation("Spec", "Goal", Solve[String, String]()))
  //    val newGraph = testGraph.addProofStep(node, Seq(LEdge("Child1", "New", NoInfoEdgeLabel)))
  //      .verifySingle(provedVerifier, "New")
  //    assert(!newGraph.computeFullyVerified("Top"))
  //    //assert(newGraph.find("Top").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  //    assert(!newGraph.computeFullyVerified("Child1"))
  //    //assert(newGraph.find("Child1").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  //  }
  //
  //  test("Both parents are set to be outdated") {
  //    val node1 = LNode("Parent", Obligation("Spec", "Goal", Solve[String, String]()))
  //    val node2 = LNode("New", Obligation("Spec", "Goal", Solve[String, String]()))
  //    val node3 = LNode("Child3", Obligation("Spec", "Goal", Solve[String, String]()))
  //
  //    val newGraph = testGraph
  //      .addProofStep(node1, Seq(LEdge("Child1", "Parent", NoInfoEdgeLabel)))
  //      .addProofStep(node3, Seq(LEdge("Top", "Child3", NoInfoEdgeLabel)))
  //      .verifySingle(provedVerifier, "Parent")
  //      .verifySingle(provedVerifier, "Child3")
  //      .addProofStep(node2, Seq(LEdge("Child2", "New", NoInfoEdgeLabel), LEdge("Parent", "New", NoInfoEdgeLabel)))
  //
  //    //assert(newGraph.find("Parent").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  //    //assert(newGraph.find("Child1").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  //    //assert(newGraph.find("Child2").get.getVerificationStatus.isInstanceOf[Outdated[String, String]])
  //    assert(!newGraph.computeFullyVerified("Parent"))
  //    assert(!newGraph.computeFullyVerified("Child1"))
  //    assert(!newGraph.computeFullyVerified("Child2"))
  //    //assert(newGraph.find("Child3").get.getVerificationStatus.isInstanceOf[Finished[String, String, String]])
  //    assert(newGraph.computeFullyVerified("Child3"))
  //  }
  //
  //  test("Verify node with children") {
  //    val node = LNode("New", Obligation("Spec", "Goal", Solve[String, String]()))
  //    val switchStatusVerifier = MockVerifier(SwitchStatusProver())
  //    val newGraph = testGraph
  //      .verifySingle(provedVerifier, "Child1")
  //      .verifySingle(provedVerifier, "Child2")
  //      .addProofStep(node, Seq(LEdge("Top", "New", NoInfoEdgeLabel)))
  //      .verifySingle(switchStatusVerifier, "Top")
  //
  //    //assert(newGraph.find("Top").get.getVerificationStatus.isVerified)
  //  }
  //
  //  test("Verify all nodes") {
  //    val node1 = LNode("Parent", Obligation("Spec", "Goal", Solve[String, String]()))
  //    val node2 = LNode("New", Obligation("Spec", "Goal", Solve[String, String]()))
  //    val node3 = LNode("Child3", Obligation("Spec", "Goal", Solve[String, String]()))
  //
  //    val newGraph = testGraph
  //      .addProofStep(node1, Seq(LEdge("Child1", "Parent", NoInfoEdgeLabel)))
  //      .addProofStep(node3, Seq(LEdge("Top", "Child3", NoInfoEdgeLabel)))
  //      .addProofStep(node2, Seq(LEdge("Child2", "New", NoInfoEdgeLabel), LEdge("Parent", "New", NoInfoEdgeLabel)))
  //      .verifyAll(provedVerifier)
  //
  //    //assert(newGraph.find("Parent").get.getVerificationStatus.isVerified)
  //    //assert(newGraph.find("New").get.getVerificationStatus.isVerified)
  //    //assert(newGraph.find("Child1").get.getVerificationStatus.isVerified)
  //    //assert(newGraph.find("Child2").get.getVerificationStatus.isVerified)
  //    //assert(newGraph.find("Child3").get.getVerificationStatus.isVerified)
  //    assert(newGraph.computeFullyVerified("Parent"))
  //    assert(newGraph.computeFullyVerified("New"))
  //    assert(newGraph.computeFullyVerified("Child1"))
  //    assert(newGraph.computeFullyVerified("Child2"))
  //    assert(newGraph.computeFullyVerified("Child3"))
  //  }
  //
  ////  test("Verify all nodes parallel") {
  ////    val node1 = LNode("Parent", ProofStep("Spec", "Goal", Solve()))
  ////    val node2 = LNode("New", ProofStep("Spec", "Goal", Solve()))
  ////    val node3 = LNode("Child3", ProofStep("Spec", "Goal", Solve()))
  ////
  ////    val waitingVerifier = MockVerifier(WaitingProver())
  ////    val newGraph = testGraph
  ////      .addNode(node1, Seq(LEdge("Child1", "Parent", NoInfoProofEdgeLabel)))
  ////      .addNode(node3, Seq(LEdge("Top", "Child3", NoInfoProofEdgeLabel)))
  ////      .addNode(node2, Seq(LEdge("Child2", "New", NoInfoProofEdgeLabel), LEdge("Parent", "New", NoInfoProofEdgeLabel)))
  ////      .verifyAllPar(waitingVerifier)
  ////
  ////    assert(newGraph.get("Parent").get.getVerificationStatus.isVerified)
  ////    assert(newGraph.get("New").get.getVerificationStatus.isVerified)
  ////    assert(newGraph.get("Child1").get.getVerificationStatus.isVerified)
  ////    assert(newGraph.get("Child2").get.getVerificationStatus.isVerified)
  ////    assert(newGraph.get("Child3").get.getVerificationStatus.isVerified)
  ////    assert(newGraph.computeFullyVerified("Parent"))
  ////    assert(newGraph.computeFullyVerified("New"))
  ////    assert(newGraph.computeFullyVerified("Child1"))
  ////    assert(newGraph.computeFullyVerified("Child2"))
  ////    assert(newGraph.computeFullyVerified("Child3"))
  ////  }
  //}
  //
  //class SimpleVerifierFormat(val s: String) extends VerifierFormat
  //
  //class SimpleStepResult extends GenStepResult[String, String] {
  //  override def status: VerifierStatus[String, String] = Finished[String, String](Proved(""), MockVerifier(MockProver()))
  //
  //  override def evidence: Option[Evidence] = None
  //
  //  override def errorMsg: Option[String] = None
  //}
  //
  //case class MockVerifier(prover: Prover[String]) extends Verifier[String, String] {
  //  override type V = SimpleVerifierFormat
  //  override val desc: String = "MockVerifier"
  //  override val supportedStrategies: Seq[Tactic[String, String]] = Seq(Solve())
  //
  //  override def verify(goal: String, spec: String, assumptions: Iterable[String]): GenStepResult[String, String] =
  //    // TODO: usedEdges cannot be passed because we only have the used goals
  //    new SimpleStepResult
}

