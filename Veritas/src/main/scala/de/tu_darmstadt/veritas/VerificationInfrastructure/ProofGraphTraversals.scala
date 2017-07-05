package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.tactic.Tactic

import scala.reflect.ClassTag

/**
  * Created by andiderp on 18/05/2017.
  */
trait ProofGraphTraversals[Spec, Goal] extends ProofGraph[Spec, Goal] {

  //TODO traversals map/filter/fold

  // think about specific scenarios
  // change every stepresult to unknown
  // change specification(type)
  // consider changing the stepresult to unknown when changing the obligation
  // create

  //  def mapObligation[NSpec, NGoal](newGraph: ProofGraph[NSpec, NGoal](fspec: Spec => NSpec, fgoal: Goal => NGoal, ftactic: Tactic[Spec, Goal] => Tactic[NSpec, NGoal]): ProofGraph[NSpec, NGoal] = {
  //    val storedObls = pg.storedObligations
  //    for ((name, obl) <- storedObls)
  //      newGraph.storeNewObligation(name, fspec(obl.spec), fgoal(obl.goal))
  //    pg.obligationDFS().foreach { case obl =>
  //      val convertedObl = newGraph.obligationProducer.newObligation(fspec(obl.spec), fgoal(obl.goal))
  //      //      addObligationAndStep(convertedObl, newGraph)
  //      val ps = newGraph.appliedStep(convertedObl)
  //      if(ps.nonEmpty) {
  ////        val convertedStepResult = newGraph.stepResultProducer.newStepResult(, None, None)
  ////        newGraph.setVerifiedBy(ps.get, )
  //      }
  //    }
  //    newGraph
  //  }

  def obligationDFS(): Seq[Obligation] = {
    val collection: scala.collection.mutable.ListBuffer[Obligation] = scala.collection.mutable.ListBuffer()

    def collectObligation(obl: Obligation): Unit = {
      collection += obl
      val ps = appliedStep(obl)
      if (ps.nonEmpty) {
        val edges = requiredObls(ps.get)
        for ((subobl, edgeLabel) <- edges) {
          collectObligation(subobl)
        }
      }
    }

    for ((_, obl) <- storedObligations) {
      collectObligation(obl)
    }
    Seq() ++ collection.distinct // only use the first occurrence of an obligation
  }

  def leaves(): Seq[Obligation] = obligations(0)

  def obligations(numberOfSubOblsAllowed: Int): Seq[Obligation] = obligationDFS().filter { obl =>
    val ps = appliedStep(obl)
    if (ps.nonEmpty) {
      val subobls = requiredObls(ps.get)
      subobls.size == numberOfSubOblsAllowed
    } else {
      numberOfSubOblsAllowed == 0
    }
  }

  def proofstepsDFS(): Seq[ProofStep] = obligationDFS().foldLeft(Seq.empty[ProofStep]) { case (result, obl) =>
    val ps = appliedStep(obl)
    if (ps.nonEmpty)
      result :+ ps.get
    else
      result
  }

  def obligationsWithTactic[T](implicit tag: ClassTag[T]): Seq[Obligation] = obligationDFS().filter{ obl =>
    val ps = appliedStep(obl)
    ps.get.tactic match {
      case _: T => true
      case _ => false
    }
  }

  def foldDFS[T](init: T)(f: (T, Obligation) => T): T = obligationDFS().foldLeft[T](init)(f)

  def mapStepResult(newGraph: ProofGraph[Spec, Goal])(f: Obligation => GenStepResult[Spec, Goal]): ProofGraph[Spec, Goal] = {
    val storedObls = storedObligations
    for ((name, obl) <- storedObls)
      newGraph.storeNewObligation(name, obl.spec, obl.goal)

    obligationDFS().foreach { case obl =>
      addObligationAndStep(obl, newGraph)
      val convertedObl = newGraph.obligationProducer.newObligation(obl.spec, obl.goal)
      val ps = newGraph.appliedStep(convertedObl)
      if(ps.nonEmpty) {
        val newStepResult = f(obl)
        val convertedStepResult = newGraph.stepResultProducer.newStepResult(newStepResult.status, newStepResult.evidence, newStepResult.errorMsg)
        newGraph.setVerifiedBy(ps.get, convertedStepResult)
      }
    }
    newGraph
  }

  private def addObligationAndStep(obl: Obligation, newGraph: ProofGraph[Spec, Goal]): Unit = {
    val convertedObl = newGraph.obligationProducer.newObligation(obl.spec, obl.goal)
    val ps = appliedStep(obl)
    if (ps.nonEmpty) {
      newGraph.applyTactic(convertedObl, ps.get.tactic)
    }
  }

  // currently user has to provide a graph because we have no abstract way of creating an empty graph
  def extractSubgraph(obl: Obligation, newGraph: ProofGraph[Spec, Goal]): ProofGraph[Spec, Goal] = {
    val convertedObl = newGraph.obligationProducer.newObligation(obl.spec, obl.goal)
    if (storedObligations.values.toSeq.contains(obl)) {
      // use name of stored obligation
      val index = storedObligations.toSeq.indexWhere { _._2 == obl }
      val oblName = storedObligations.toSeq(index)._1
      newGraph.storeObligation(oblName, convertedObl)
    } else {
      newGraph.storeObligation("subgraphRoot", convertedObl)
    }
    constructChildren(obl, newGraph)
    newGraph
  }

  private def constructChildren(obl: Obligation, newGraph: ProofGraph[Spec, Goal]): Unit = {
    val convertedObl = newGraph.obligationProducer.newObligation(obl.spec, obl.goal)
    val ps = appliedStep(obl)
    if (ps.nonEmpty) {
      val tactic = ps.get.tactic
      newGraph.applyTactic(convertedObl, tactic)
      val obls = requiredObls(ps.get).map { _._1 }
      for (subobl <- obls)
        constructChildren(subobl, newGraph)
    }
  }
}
