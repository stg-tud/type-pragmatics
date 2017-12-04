package de.tu_darmstadt.veritas.VerificationInfrastructure

import de.tu_darmstadt.veritas.VerificationInfrastructure.verifier.{Transformer, VerifierFormat}
import de.tu_darmstadt.veritas.backend.ast._

/**
  * UI for proof graph ->
  * more comfortable access to subobligation (e.g. via names)
  *
  * @param goalDescriptor extracting a descriptive string from any Goal (e.g. with VeritasConstruct:
  *                       goal name, lemma name)
  */
class ProofGraphUI[Spec, Goal](val pg: ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal],
                               val goalDescriptor: Goal => String) {


  protected var obligationMap: Map[pg.Obligation, String] = calcObligationMap()

  private var latestHash = pg.hashCode()

  /**
    * Only creates a mapping if the goalDescriptor is able to extract unique names
    *
    * @return returns a map from obligation to unique name.
    */
  protected def calcObligationMap(): Map[pg.Obligation, String] = {
    val seenNames = scala.collection.mutable.ListBuffer[String]()
    pg.obligationDFS().map { obl =>
      val newName = goalDescriptor(obl.goal)
      if (seenNames.contains(newName))
        throw new IllegalArgumentException("goalDescriptor was not able to extract unique names")
      else
        seenNames += newName
      (obl, newName)
    }.toMap
  }

  protected def recalcObligationMap(): Unit = {
    if (latestHash != pg.hashCode()) {
      latestHash = pg.hashCode()
      obligationMap = calcObligationMap()
    }
  }

  def getObligation(name: String): pg.Obligation = {
    recalcObligationMap()
    val filteredObls = obligationMap.filter { case (obl, n) =>
      n == name
    }.toSeq
    filteredObls.head._1
  }

  def getNameOfObligation(obl: pg.Obligation): String = {
    recalcObligationMap()
    obligationMap(obl)
  }

  def getAssembledProblem[V <: VerifierFormat](obl: pg.Obligation, transformer: Transformer[Spec, Goal, V]): (Spec, Spec, Goal) = {
    val parentedges = pg.requiringSteps(obl) map (_._2)
    val ps = pg.appliedStep(obl).get
    val assumptions = pg.requiredObls(ps) map { case (o, el) => (el, o.goal) }
    transformer.assembleFullProblem(obl.goal, obl.spec, parentedges, assumptions)
  }
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
