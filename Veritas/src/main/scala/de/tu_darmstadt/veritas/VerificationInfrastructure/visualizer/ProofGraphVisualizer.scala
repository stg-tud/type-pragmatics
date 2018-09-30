package de.tu_darmstadt.veritas.VerificationInfrastructure.visualizer

import de.tu_darmstadt.veritas.VerificationInfrastructure.{EdgeLabel, ProofGraph}

trait ProofGraphVisualizer[Output, Spec, Goal] {
  val graph: ProofGraph[Spec, Goal]

  protected val obligations = scala.collection.mutable.ListBuffer[(String, graph.Obligation)]()
  protected val proofsteps = scala.collection.mutable.ListBuffer[graph.ProofStep]()
  protected val fromObligation = scala.collection.mutable.Map[graph.Obligation, graph.ProofStep]()
  protected val fromProofstep= scala.collection.mutable.Map[graph.ProofStep, Seq[(graph.Obligation, EdgeLabel)]]()

  protected def collect(): Unit = {
    for ((name, obl) <- graph.storedObligations) {
      collect(name, obl)
    }
  }

  protected def collect(name: String, obl: graph.Obligation): Unit = {
    val obligationVisited = obligations.find(_._2.goal == obl.goal).nonEmpty
    if (obligationVisited)
      return

    obligations += (name -> obl)
    val ps = graph.appliedStep(obl)
    if (ps.nonEmpty) {
      fromObligation += (obl -> ps.get)
      proofsteps += ps.get
      val edges = graph.requiredObls(ps.get).toSeq
      fromProofstep += (ps.get -> edges)
      for ((subobl, edgeLabel) <- edges) {
        // TODO: how should the subobligation be named?
        val obligationVisited = obligations.find(_._2.goal == subobl.goal).nonEmpty
        if (!obligationVisited)
          collect(subobl.problemName, subobl)
      }
    }
  }

  def visualize(): Output = {
    collect()
    for ((name, obl) <- obligations.distinct) {
      encodeObligation(obl)
    }
    for (ps <- proofsteps) {
      encodeProofStep(ps)
    }
    for ((obl, ps) <- fromObligation) {
      linkToProofStep(obl, ps)
    }
    for ((ps, edges) <- fromProofstep) {
      for (e <- edges) {
        linkFromProofStep(ps, e._1, e._2)
      }
    }
    getResult()
  }

  def getResult(): Output

  def encodeObligation(obl: graph.Obligation): Unit
  def encodeProofStep(ps: graph.ProofStep): Unit
  def linkToProofStep(obl: graph.Obligation, ps: graph.ProofStep): Unit
  def linkFromProofStep(ps: graph.ProofStep, obl: graph.Obligation, edgeLabel: EdgeLabel): Unit
}
