package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.backend.ast._

/**
  * UI for proof graph ->
  * more comfortable access to subobligation (e.g. via names)
  * traversals
  *
  * @param goalDescriptor extracting a descriptive string from any Goal (e.g. with VeritasConstruct:
  *                 goal name, lemma name)
  */
class ProofGraphUI[Spec, Goal](val pg: ProofGraph[Spec, Goal],
                              val goalDescriptor: Goal => String) {


  // TODO: should it be possible that a name can map to multiple obligations, the otherway around or a 1 to 1 mapping?
  // currently a 1 to 1 mapping
  // automatically check if thats the case
  protected val obligationMap: Map[pg.Obligation, String] = obligationDFS().map { obl => (obl, goalDescriptor(obl.goal)) }.toMap


  def obligationDFS(): Seq[pg.Obligation] = {
    val collection: scala.collection.mutable.ListBuffer[pg.Obligation] = scala.collection.mutable.ListBuffer()

    def collectObligation(obl: pg.Obligation): Unit = {
      collection += obl
      val ps = pg.appliedStep(obl)
      if (ps.nonEmpty) {
        val edges = pg.requiredObls(ps.get)
        for ((subobl, edgeLabel) <- edges) {
          collectObligation(subobl)
        }
      }
    }

    for ((_, obl) <- pg.storedObligations) {
      collectObligation(obl)
    }
    Seq() ++ collection.distinct // only use the first occurrence of an obligation
  }

  def proofstepsDFS(): Seq[pg.ProofStep] = obligationDFS().foldLeft(Seq.empty[pg.ProofStep]) { case (result, obl) =>
    val ps = pg.appliedStep(obl)
    if (ps.nonEmpty)
      result :+ ps.get
    else
      result
  }

  def getObligation(name: String): pg.Obligation = {
    val filteredObls = obligationMap.filter { case (obl, n) =>
       n == name
    }.toSeq
    filteredObls.head._1
  }

  def getNameOfObligation(obl: pg.Obligation): String = {
    obligationMap(obl)
  }

  // currently user has to provide a graph because we have no abstract way of creating an empty graph
  def extractSubgraph(obl: pg.Obligation, newGraph: ProofGraph[Spec, Goal]): ProofGraph[Spec, Goal] = {
    val convertedObl = newGraph.obligationProducer.newObligation(obl.spec, obl.goal)
    if (pg.storedObligations.values.toSeq.contains(obl)) {
      // use name of stored obligation
      val index = pg.storedObligations.toSeq.indexWhere { _._2 == obl }
      val oblName = pg.storedObligations.toSeq(index)._1
      newGraph.storeObligation(oblName, convertedObl)
    } else {
      newGraph.storeObligation("subgraphRoot", convertedObl)
    }
    constructChildren(obl, newGraph)
    newGraph
  }

  private def constructChildren(obl: pg.Obligation, newGraph: ProofGraph[Spec, Goal]): Unit = {
    val convertedObl = newGraph.obligationProducer.newObligation(obl.spec, obl.goal)
    val ps = pg.appliedStep(obl)
    if (ps.nonEmpty) {
      newGraph.applyTactic(convertedObl, ps.get.tactic)
      val obls = pg.requiredObls(ps.get).map { _._1 }
      for (obl <- obls)
        constructChildren(obl, newGraph)
    }
  }

  //TODO traversals map/filter/fold

  // think about specific scenarios
  // change every stepresult to unknown
  // change specification(type)
  // consider changing the stepresult to unknown when changing the obligation
  // create

//  def mapObligation[NSpec, NGoal](newGraph: ProofGraph[NSpec, NGoal](fspec: Spec => NSpec, fgoal: Goal => NGoal): ProofGraph[NSpec, NGoal] = {
//    val storedObls = pg.storedObligations
//    for ((name, obl) <- storedObls)
//      newGraph.storeNewObligation(name, fspec(obl.spec), fgoal(obl.goal))
//    obligationDFS().foreach { case obl =>
//      val convertedObl = newGraph.obligationProducer.newObligation(fspec(obl.spec), fgoal(obl.goal))
////      addObligationAndStep(convertedObl, newGraph)
//      val ps = newGraph.appliedStep(convertedObl)
//      if(ps.nonEmpty) {
//        val convertedStepResult = newGraph.stepResultProducer.newStepResult(, None, None)
//        newGraph.setVerifiedBy(ps.get, )
//      }
//    }
//    newGraph
//  }

  def mapStepResult(newGraph: ProofGraph[Spec, Goal])(f: Option[pg.StepResult] => GenStepResult[Spec, Goal]): ProofGraph[Spec, Goal] = {
    val storedObls = pg.storedObligations
    for ((name, obl) <- storedObls)
      newGraph.storeNewObligation(name, obl.spec, obl.goal)

    obligationDFS().foreach { case obl =>
      addObligationAndStep(obl, newGraph)
      val convertedObl = newGraph.obligationProducer.newObligation(obl.spec, obl.goal)
      val ps = newGraph.appliedStep(convertedObl)
      if(ps.nonEmpty) {
        val prevStepResult = pg.verifiedBy(pg.appliedStep(obl).get)
        val newStepResult = f(prevStepResult)
        val convertedStepResult = newGraph.stepResultProducer.newStepResult(newStepResult.status, newStepResult.evidence, newStepResult.errorMsg)
        newGraph.setVerifiedBy(ps.get, convertedStepResult)
      }
    }
    newGraph
  }

  private def addObligationAndStep(obl: pg.Obligation, newGraph: ProofGraph[Spec, Goal]): Unit = {
    val convertedObl = newGraph.obligationProducer.newObligation(obl.spec, obl.goal)
    val ps = pg.appliedStep(obl)
    if (ps.nonEmpty) {
      newGraph.applyTactic(convertedObl, ps.get.tactic)
    }
  }

  def foldDFS[T](init: T)(f: (T, pg.Obligation) => T): T = obligationDFS().foldLeft[T](init)(f)

}

object ProofGraphUI {
   val extractGoalName: VeritasConstruct => String = _ match {
    case Goals(goals, _) => goals.head.name // Veritas only supports a single goal
    case GoalsWithStrategy(_, goals, _) => goals.head.name // Veritas only supports a single goal
    case _ => throw new IllegalArgumentException("The goal of the obligation is not a real Goal")
  }

  val extractGoalOrLemmaName: VeritasConstruct => String = _ match {
    case Goals(goals, _) => goals.head.name // Veritas only supports a single goal
    case GoalsWithStrategy(_, goals, _) => goals.head.name // Veritas only supports a single goal
    case Lemmas(goals, _) => goals.head.name
    case LemmasWithStrategy(_, goals, _) => goals.head.name
    case _ => throw new IllegalArgumentException("The goal of the obligation is not a real Goal or Lemma")
  }
}
