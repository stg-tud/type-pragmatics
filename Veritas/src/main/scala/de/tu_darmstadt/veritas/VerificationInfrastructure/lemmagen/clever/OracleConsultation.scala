package de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.clever

import de.tu_darmstadt.veritas.VerificationInfrastructure.lemmagen.{Oracle, Problem}

import scala.collection.mutable

class OracleConsultation(val problem: Problem) {
  def updateStatus(node: RefinementNode): ProvabilityStatus = {
    println(node.lemma)
    Oracle.invoke(problem, Set(node.lemma)) match {
      case Oracle.Inconclusive() => Inconclusive()
      case Oracle.ProvablyFalse(_) => Disproved()
    }
  }

  def consult(tree: RefinementGraph): Unit = {
    var fringe = new mutable.Queue[RefinementNode]()
    fringe ++= tree.leaves
    while(fringe.nonEmpty) {
      var node = fringe.dequeue()
      fringe ++= node.parents
      while(node.provabilityStatus != Unknown() && fringe.nonEmpty) {
        node = fringe.dequeue()
        fringe ++= node.parents
      }
      println(s"${fringe.size} elements")
      if(node.provabilityStatus == Unknown()) {
        val status = updateStatus(node)
        if (status == Disproved()) {
          node.setProvabilityStatusRecursively(status)
        } else {
          node.provabilityStatus = status
        }
      }
    }
  }
}
