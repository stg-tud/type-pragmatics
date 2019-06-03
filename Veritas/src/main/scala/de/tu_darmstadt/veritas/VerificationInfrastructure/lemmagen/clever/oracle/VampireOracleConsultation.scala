package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever.oracle

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever._
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.LemmaApplicationStep
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{Finished, Proved, TPTPVampireVerifier, VerifierStatus}
import de.tu_darmstadt.veritas.VerificationInfrastructure.{Evidence, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpFalse

import scala.collection.mutable

/** Oracle consultation using Vampire and falsity propagation. */
class VampireOracleConsultation(val problem: Problem) extends OracleConsultation {
  class StepResult(val status: VerifierStatus[VeritasConstruct, VeritasFormula],
                   val evidence: Option[Evidence], val errorMsg: Option[String])
    extends GenStepResult[VeritasConstruct, VeritasFormula]  {
  }

  object stepResultProducer extends StepResultProducer[VeritasConstruct, VeritasFormula, StepResult] {
    override def newStepResult(status: VerifierStatus[VeritasConstruct, VeritasFormula], evidence: Option[Evidence],
                               errorMsg: Option[String]): StepResult = {
      new StepResult(status, evidence, errorMsg)
    }
  }

  /** Invoke Vampire 4.2.2 on `problem`, with additional lemmas and a timeout, and try to prove `false`. */
  def invokeOracle(problem: Problem, lemmas: Set[Lemma],
                   timeout: Integer = 3, logic: String = "tff"): ProvabilityStatus = {
    val falseGoal = Goals(Seq(TypingRule("false-goal", Seq(), Seq(FunctionExpJudgment(FunctionExpFalse)))), None)
    val verifier = new TPTPVampireVerifier(timeout, "4.2.2", logic)
    val assumptions = lemmas.map(lemma => (LemmaApplicationStep(lemma.name), lemma)).toSeq
    // sanity check for distinguishable names
    require(lemmas.map(_.name).size == lemmas.size)
    val result = verifier.verify(
      falseGoal,
      problem.spec,
      Seq(),
      assumptions,
      None,
      stepResultProducer
    )
    result.status match {
      case Finished(Proved(_), _) => DirectlyDisproved()
      case _ => Inconclusive()
    }
  }

  sealed trait NodeColor
  case class Gray() extends NodeColor
  case class White() extends NodeColor

  /** Compute a topological ordering of the transpose of `graph`.
    * This implements a variant of DFS(G) from CLRS.
    */
   def topologicalOrderOfTranspose(graph: RefinementGraph): Seq[RefinementNode] = {
    val order = new mutable.ListBuffer[RefinementNode]()
    val colors = new mutable.HashMap[RefinementNode, NodeColor]()
    // initialize all noes to white
    for(node <- graph.nodes)
      colors(node) = White()
    for(node <- graph.nodes)
      if(colors(node) == White())
        dfsVisit(node)

    def dfsVisit(u: RefinementNode): Unit = {
      colors(u) = Gray()
      // in contrast to CLRS, consider all *incoming* edges because we want
      // a topological sort of the *transpose* of graph
      for(v <- u.ancestors)
        if(colors(v) == White())
          dfsVisit(v)
      order.prepend(u)
    }

    // sanity check
    require(order.size == graph.nodes.size)
    require(order.toSet == graph.nodes)
    order
  }

  /** Consult the oracle and perform falsity propagation. */
  override def consult(graph: RefinementGraph): Unit = {
    for(node <- topologicalOrderOfTranspose(graph)) {
      if(node.provabilityStatus == Unknown()) {
        val status = invokeOracle(problem, Set(node.lemma))
        if (status == DirectlyDisproved()) {
          graph.setDisprovedStatusRecursively(node)
        } else {
          node.provabilityStatus = status
        }
      }
    }
  }
}
