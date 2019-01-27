package de.tu_darmstadt.veritas.VerificationInfrastructure.strategies
import de.tu_darmstadt.veritas.scalaspl.util.AugmentedCallGraph

/**
  * simply select the first progress and the first preservation property for a given function name, if present in domain-specific knowledge
  * @tparam Type
  * @tparam FDef
  * @tparam Prop
  * @tparam Equation
  * @tparam Criteria
  * @tparam Expression
  */
case class SimplyFirstSelectionStrategy[Type, FDef, Prop, Equation, Criteria, Expression]() extends LemmaSelectionStrategy[Type, FDef, Prop, Equation, Criteria, Expression] {
  override def selectLemma(dsk: DomainSpecificKnowledge[Type, FDef, Prop], acg: AugmentedCallGraph[Equation, Criteria, Expression], fn: String): Seq[Prop] = {
    val progresslemmas = dsk.lookupByFunName(dsk.progressProperties, fn)
    val preservationlemmas = dsk.lookupByFunName(dsk.preservationProperties, fn)

    val progresslemma = if (progresslemmas.nonEmpty) Seq(progresslemmas.head) else Seq()
    val preservationlemma = if (preservationlemmas.nonEmpty) Seq(preservationlemmas.head) else Seq()

    progresslemma ++ preservationlemma

  }
}
