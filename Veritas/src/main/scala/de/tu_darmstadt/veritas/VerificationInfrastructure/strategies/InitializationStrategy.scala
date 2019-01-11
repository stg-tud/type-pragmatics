package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}

/**
  * top-level strategy for initializing a proof graph, given the name of a dynamic function
  */
trait InitializationStrategy[Spec, Goal] {
  val g: ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal]

  //initialize a proof graph from a given fully constructed goal
  def initializePG(g: Goal): ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal] = initializePG(Set(g))

  //initialize a proof graph by constructing a goal from a given name (e.g. of a function or of an axiom etc.)
  def initializePGfromName(fn: String): ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal]

  //initialize a proof graph with several roots from a given set of fully constructed goals
  def initializePG(gs: Set[Goal]): ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal]

  def getGoalsFromFunName(fn: String): Set[Goal]
}
