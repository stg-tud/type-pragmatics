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
  protected val obligationMap: Map[pg.Obligation, String] = dfs().map { obl => (obl, goalDescriptor(obl.goal)) }.toMap

  private def dfs(): Seq[pg.Obligation] = {
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
    Seq() ++ collection
  }

  def getObl(name: String): pg.Obligation = {
    val filteredObls = obligationMap.filter { case (obl, n) =>
       n == name
    }.toSeq
    filteredObls.head._1
  }

  def getNameOfObl(obl: pg.Obligation): String = {
    obligationMap(obl)
  }

  //TODO traversals map/filter/fold
}

object ProofGraphUI {
  def extractGoalName(vc: VeritasConstruct): String = vc match {
    case Goals(goals, _) => goals.head.name // Veritas only supports a single goal
    case GoalsWithStrategy(_, goals, _) => goals.head.name // Veritas only supports a single goal
    case _ => throw new IllegalArgumentException("The goal of the obligation is not a real Goal")
  }

  def extractGoalOrLemmaName(vc: VeritasConstruct): String = vc match {
    case Goals(goals, _) => goals.head.name // Veritas only supports a single goal
    case GoalsWithStrategy(_, goals, _) => goals.head.name // Veritas only supports a single goal
    case Lemmas(goals, _) => goals.head.name
    case LemmasWithStrategy(_, goals, _) => goals.head.name
    case _ => throw new IllegalArgumentException("The goal of the obligation is not a real Goal or Lemma")
  }
}
