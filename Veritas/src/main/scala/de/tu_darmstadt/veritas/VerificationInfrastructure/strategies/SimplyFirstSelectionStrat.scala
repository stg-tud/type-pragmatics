package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies

import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

/**
  * simply select the first progress and the first preservation property for a given function name, if present in domain-specific knowledge
  *
  * @tparam Type
  * @tparam FDef
  * @tparam Prop
  * @tparam Equation
  * @tparam Criteria
  * @tparam Expression
  */
case class SimplyFirstSelectionStrat[
Type,
FDef,
Prop,
Equation,
Criteria,
Expression]()
  extends LemmaSelectionStrat[Type, FDef, Prop, Equation, Criteria, Expression] {
  override def selectLemma(dsk: DomainSpecificKnowledge[Type, FDef, Prop], acg: AugmentedCallGraph[Equation, Criteria, Expression], fn: String): Seq[Prop] = {
    val progresslemmas = dsk.lookupByFunName(dsk.progressProperties, fn)
    val preservationlemmas = dsk.lookupByFunName(dsk.preservationProperties, fn)
    val otherlemmas = dsk.lookupByFunName(dsk.auxiliaryProperties, fn)

    val progresslemma = if (progresslemmas.nonEmpty) Seq(progresslemmas.head) else Seq()
    val preservationlemma = if (preservationlemmas.nonEmpty) Seq(preservationlemmas.head) else Seq()
    val otherlemma = if (otherlemmas.nonEmpty) Seq(otherlemmas.head) else Seq()

    progresslemma ++ preservationlemma ++ otherlemma

  }
}
