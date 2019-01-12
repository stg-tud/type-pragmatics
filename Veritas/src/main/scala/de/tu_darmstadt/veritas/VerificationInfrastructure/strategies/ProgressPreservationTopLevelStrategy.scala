package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.VerificationInfrastructure.{ProofGraph, ProofGraphTraversals}
import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

trait ProgressPreservationTopLevelStrategy[Spec, Goal, Type, FDef, Prop, Equation, Criteria, Expression]
  extends InitializationStrategy[Spec, Goal] {

  lazy val dsk = computeDomainSpecificKnowledge() //globally compute domain specific knowledge as implemented in concrete class
  //important: has to be lazy so that it is only computed when it is actually needed within the methods (NullPointerExcepections may
  //arise otherwise since at this point in the execution, not everything may have been initialized

  //convenience function for top-level graph generation (from reduce function on)
  def generateFullGraph(): ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal] = {
    // we currently assume that the top-level function for the reduction semantics is always named "reduce"
    // later, one could automatically detect this be finding out that reduce is not called by any other function in the specification
    initializePGfromName("reduce")
  }

  override def initializePGfromName(fn: String): ProofGraph[Spec, Goal] with ProofGraphTraversals[Spec, Goal] = {
    // add root properties for function from super class
    val goals = for (goal <- getGoalsFromFunName(fn)) yield goal
    initializePG(goals)

    val rootstrat = new ApplyStratToRoots[Spec, Goal](ProgressPreservationBasicLoop(dsk, createACG))
    rootstrat.applyToPG(g)(g.storedObligations.values.head) //the given obligation does not matter here

    g
  }

  def computeDomainSpecificKnowledge(): DomainSpecificKnowledge[Type, FDef, Prop]
  def createACG(fn: String): AugmentedCallGraph[Equation, Criteria, Expression]

}
