package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.{Evidence, GenStepResult, StepResultProducer}
import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Lemma, Problem}
import de.tu_darmstadt.veritas.VerificationInfrastructure.tactics.LemmaApplicationStep
import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{Finished, Proved, TPTPVampireVerifier, VerifierStatus}
import de.tu_darmstadt.veritas.backend.ast._
import de.tu_darmstadt.veritas.backend.ast.function.FunctionExpFalse

import scala.collection.mutable

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

  def invokeOracle(problem: Problem, lemmas: Set[Lemma], timeout: Integer = 3, logic: String = "tff"): ProvabilityStatus = {
    val falseGoal = Goals(Seq(TypingRule("false-goal", Seq(), Seq(FunctionExpJudgment(FunctionExpFalse)))), None)
    val verifier = new TPTPVampireVerifier(timeout, "4.2.2", logic)
    //val verifier = new ADTVampireVerifier(timeout)
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

  def consult(graph: RefinementGraph): Unit = {
    var fringe = new mutable.Queue[RefinementNode]()
    fringe ++= graph.leaves
    while(fringe.nonEmpty) {
      var node = fringe.dequeue()
      fringe ++= node.parents
      while(node.provabilityStatus != Unknown() && fringe.nonEmpty) {
        node = fringe.dequeue()
        fringe ++= node.parents
      }
      if(node.provabilityStatus == Unknown()) {
        println(node.lemma)
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
